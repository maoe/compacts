{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.SubWord
  ( SubWord(..)
  ) where

-- | A class of data types that are smaller than a certain word
-- (i.e. @'WordX' a@) size.
--
class SubWord a where
  type WordX a

  castToWord :: a -> WordX a
  default castToWord :: (Integral a, Num (WordX a)) => a -> WordX a
  castToWord = fromIntegral
