module Brutus.LogicalModel.Enumerable
  ( Enumerable(..),
  ) where

class Enumerable p where
  enumerated :: [p]


