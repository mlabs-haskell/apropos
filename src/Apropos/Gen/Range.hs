{- |
Module: Apropos.Gen.Range
Maintainer: fraser@mlabs.city
Description: TODO

TODO
-}
module Apropos.Gen.Range (
  Range (..),
  linear,
  singleton,
  rangeSize,
  rangeHi,
  rangeLo,
) where

{- | Data type representing a linear range of `Int`s or
     a single `Int`.
-}
data Range
  = -- | A linear range from one `Int` to another.
    Linear Int Int
  | -- | A single `Int`.
    Singleton Int

-- Given two `Int`s, creates a `Range` from one to the other.
linear :: Int -> Int -> Range
linear = Linear

-- Given a single `Int`, creates a `Range` for that value.
singleton :: Int -> Range
singleton = Singleton

rangeSize :: Range -> Int
rangeSize (Singleton _) = 1
rangeSize (Linear lo hi) = 1 + fromIntegral (max 0 (hi - lo))

rangeHi :: Range -> Int
rangeHi (Singleton s) = fromIntegral s
rangeHi (Linear _ hi) = fromIntegral hi

rangeLo :: Range -> Int
rangeLo (Singleton s) = fromIntegral s
rangeLo (Linear lo _) = fromIntegral lo
