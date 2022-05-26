module Apropos.Gen.Range (
  Range (..),
  linearFrom,
  linear,
  singleton,
  rangeSize,
  rangeHi,
  rangeLo,
) where

data Range a
  = LinearFrom a a a
  | Linear a a
  | Singleton a

linearFrom :: a -> a -> a -> Range a
linearFrom = LinearFrom

linear :: a -> a -> Range a
linear = Linear

singleton :: a -> Range a
singleton = Singleton

rangeSize :: (Ord a, Num a) => Range a -> a
rangeSize (Singleton _) = 1
rangeSize (Linear lo hi) = 1 + max 0 (hi - lo)
rangeSize (LinearFrom _ lo hi) = 1 + max 0 (hi - lo)

rangeHi :: Range a -> a
rangeHi (Singleton s) = s
rangeHi (Linear _ hi) = hi
rangeHi (LinearFrom _ _ hi) = hi

rangeLo :: Range a -> a
rangeLo (Singleton s) = s
rangeLo (Linear lo _) = lo
rangeLo (LinearFrom _ lo _) = lo
