{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.CompactArray
  ( CompactArray
  , fromUnboxedVector
  , index
  , unsafeIndex
  ) where
import Control.Monad.ST
import Data.Bits
import Data.Proxy
import GHC.TypeLits

import Control.Monad.Primitive
import Data.Tagged
import Data.Primitive
import qualified Data.Primitive.ByteArray as P
import qualified Data.Vector.Unboxed as VU

import Data.SubWord

-- | A static compressed array which has elements of fixed size @l@.
data CompactArray (l :: Nat) a = CompactArray
  { arr :: {-# UNPACK #-} !P.ByteArray
  -- ^ Internal representation of the array
  , len :: {-# UNPACK #-} !Int
  -- ^ The number of elements in the internal representation
  }

fromUnboxedVector
  :: forall l a.
    ( KnownNat l, VU.Unbox a
    , SubWord a, Num (WordX a), Bits (WordX a), Prim (WordX a)
    )
  => VU.Vector a
  -> CompactArray l a
fromUnboxedVector vec = CompactArray {..}
  where
    arr = runST $ do
      marr <- P.newByteArray cap
      VU.imapM_ (write (Tagged @l marr)) vec
      P.unsafeFreezeByteArray marr
    len = VU.length vec
    l = fromIntegral $ natVal (Proxy :: Proxy l)
    cap = l * len `divUp` w

index :: CompactArray l a -> Int -> a
index = undefined

unsafeIndex :: CompactArray l a -> Int -> a
unsafeIndex = undefined

write
  :: forall m l a.
    ( PrimMonad m, KnownNat l
    , SubWord a, Num (WordX a), Bits (WordX a), Prim (WordX a)
    )
  => Tagged l (P.MutableByteArray (PrimState m))
  -> Int
  -> a
  -> m ()
write (Tagged marr) i a
  | j `div` w == k `div` w = do
    let
      mask :: WordX a
      mask = complement $ ((1 `shiftL` (k - j + 1)) - 1) `shiftL` (j `mod` w)
    modifyByteArray marr (j `div` w) $ \word -> word .&. mask
    modifyByteArray marr (j `div` w) $ \word ->
      word .|. (castToWord a `shiftL` (j `mod` w))
  | otherwise = do
    modifyByteArray marr (j `div` w) $ \word ->
      word .&. ((1 `shiftL` (j `mod` w)) - 1)
        .|. (castToWord a `shiftL` (j `mod` w))
    modifyByteArray marr (k `div` w) $ \word ->
      word .&. complement ((1 `shiftL` ((k + 1) `mod` w)) - 1)
        .|. (castToWord a `shiftR` (w - (j `mod` w)))
  where
    l = fromIntegral $ natVal (Proxy :: Proxy l)
    j = i * l
    k = (i + 1) * l - 1

w :: Int
w = finiteBitSize (undefined :: Word)

-- | Integer devision that rounds up
divUp :: Int -> Int -> Int
divUp x y = negate $ (-x) `div` y

modifyByteArray
  :: (PrimMonad m, Prim a)
  => P.MutableByteArray (PrimState m)
  -> Int
  -> (a -> a)
  -> m ()
modifyByteArray marr i f = do
  a <- P.readByteArray marr i
  P.writeByteArray marr i $! f a
