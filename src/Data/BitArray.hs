{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Data.BitArray
  ( BitArray
  , fromShortByteString
  , access
  ) where
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Word

import Control.Monad.Primitive
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Short.Internal as BS (ShortByteString(SBS))
import qualified Data.Primitive.ByteArray as P

import Data.Bits.Reverse (reverseWord8)

-- $setup
-- >>> :set -XBinaryLiterals

-- | A static uncompressed bit array
data BitArray = BitArray
  { arr :: {-# UNPACK #-} !P.ByteArray
  -- ^ Internal representaion of the BitArray.
  --
  -- The bits are deployed in reversed order for each byte.
  , bits :: {-# UNPACK #-} !Int
  -- ^ The number of valid bits in the internal representation
  }

-- |
-- >>> import qualified Data.ByteString.Short as BS
-- >>> import Data.Maybe
-- >>> let bv = fromShortByteString $ BS.pack [0b10100000, 0b00010010]
-- >>> print $ mapMaybe (access bv) [0..15]
-- [True,False,True,False,False,False,False,False,False,False,False,True,False,False,True,False]
fromShortByteString :: ShortByteString -> BitArray
fromShortByteString bytes@(BS.SBS arr#) = BitArray {..}
  where
    arr = runST $ do
      marr <- P.newByteArray bytesLen
      copyReversedByteArray (P.ByteArray arr#) marr
      P.unsafeFreezeByteArray marr
    bits = bytesLen * finiteBitSize (undefined :: Word8)
    bytesLen = BS.length bytes

copyReversedByteArray
  :: PrimMonad m
  => P.ByteArray
  -> P.MutableByteArray (PrimState m)
  -> m ()
copyReversedByteArray arr marr = go 0
  where
    go i = when (i < P.sizeofByteArray arr) $
      P.writeByteArray @Word32 marr i $
        reversedByteAt (i * 4 + 0) `shiftL` 0
          .|. reversedByteAt (i * 4 + 1) `shiftL` 8
          .|. reversedByteAt (i * 4 + 2) `shiftL` 16
          .|. reversedByteAt (i * 4 + 3) `shiftL` 24
    reversedByteAt :: Int -> Word32
    reversedByteAt = fromIntegral . reverseWord8 . P.indexByteArray arr

-- |
-- >>> import qualified Data.ByteString.Short as BS
-- >>> bv = fromShortByteString $ BS.pack [0b01100000]
-- >>> mapM_ (print . access bv) [0..8]
-- Just False
-- Just True
-- Just True
-- Just False
-- Just False
-- Just False
-- Just False
-- Just False
-- Nothing
access :: BitArray -> Int -> Maybe Bool
access bv@BitArray {..} i = do
  guard $ i < bits
  return $! unsafeAccess bv i

unsafeAccess :: BitArray -> Int -> Bool
unsafeAccess BitArray {arr} i = block .&. (1 `shiftL` b) /= 0
  where
    block :: Word32
    block = P.indexByteArray arr w
    (w, b) = i `quotRem` finiteBitSize block
