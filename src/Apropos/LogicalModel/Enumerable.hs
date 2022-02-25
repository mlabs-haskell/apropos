module Apropos.LogicalModel.Enumerable (
  Enumerable (..),
) where

class (Eq p, Ord p) => Enumerable p where
  enumerated :: [p]
