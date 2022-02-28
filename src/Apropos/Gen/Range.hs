module Apropos.Gen.Range (
  Range(..),
  linear,
  singleton,
  rangeSize,
  rangeHi,
  rangeLo,
  ) where

data Range = Linear Int Int | Singleton Int

linear :: Int -> Int -> Range
linear a b = Linear a b

singleton :: Int -> Range
singleton s = Singleton s

rangeSize :: Range -> Int
rangeSize (Singleton _) = 1
rangeSize (Linear lo hi) = 1 + fromIntegral (max 0 (hi - lo))

rangeHi :: Range -> Int
rangeHi (Singleton s) = fromIntegral s
rangeHi (Linear _ hi) = fromIntegral hi

rangeLo :: Range -> Int
rangeLo (Singleton s) = fromIntegral s
rangeLo (Linear lo _) = fromIntegral lo


