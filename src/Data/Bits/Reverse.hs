{-# LANGUAGE CPP #-}
module Data.Bits.Reverse (reverseWord8) where
import Data.Word

import qualified Data.Primitive.Array as P

-- The bits reversal trick taken from
-- <http://graphics.stanford.edu/~seander/bithacks.html#BitReverseTable>.
#define R2(n)    n ,    n + 2*64 ,    n + 1*64 ,    n + 3*64
#define R4(n) R2(n), R2(n + 2*16), R2(n + 1*16), R2(n + 3*16)
#define R6(n) R4(n), R4(n + 2*4 ), R4(n + 1*4 ), R4(n + 3*4 )

bitReverseTable256 :: P.Array Word8
bitReverseTable256 = P.fromListN 256
  [ R6(0)
  , R6(2)
  , R6(1)
  , R6(3)
  ]

reverseWord8 :: Word8 -> Word8
reverseWord8 = P.indexArray bitReverseTable256 . fromIntegral
