module Data.BitVector.ZeroOrder
  ( BitVector
  ) where
import qualified Data.Primitive.ByteArray as P

import Data.BitArray

-- | A static compressed bitvector using zero-order compression.
data BitVector = BitVector
  { classes :: {-# UNPACK #-} !P.ByteArray
  , offsets :: {-# UNPACK #-} !P.ByteArray
  }
