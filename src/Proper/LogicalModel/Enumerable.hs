module Proper.LogicalModel.Enumerable
  ( Enumerable(..),
  ) where

class Enumerable p where
  enumerated :: [p]


