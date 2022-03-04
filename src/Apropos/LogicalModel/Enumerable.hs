{-# LANGUAGE DefaultSignatures #-}

module Apropos.LogicalModel.Enumerable (
  Enumerable (..),
) where

class (Eq p, Ord p) => Enumerable p where
  enumerated :: [p]
  default enumerated :: (Enum p, Bounded p) => [p]
  enumerated = [minBound .. maxBound]
