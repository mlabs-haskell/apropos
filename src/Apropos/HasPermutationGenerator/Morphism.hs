{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.Morphism (
  Morphism (..),
  (&&&),
  (>>>),
) where

import Apropos.Gen
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Control.Monad ((>=>))

data Morphism p m = Morphism
  { name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , morphism :: m -> Gen m
  }

instance Eq (Morphism p m) where
  (==) a b = name a == name b

instance Show (Morphism p m) where
  show = name

(&&&) :: [Morphism p m] -> [Morphism p m] -> [Morphism p m]
(&&&) as bs = [addMorphism a b | a <- as, b <- bs]

(>>>) :: Enumerable p => [Morphism p m] -> [Morphism p m] -> [Morphism p m]
(>>>) as bs = [seqMorphism a b | a <- as, b <- bs]

addMorphism :: Morphism p m -> Morphism p m -> Morphism p m
addMorphism a b =
  Morphism
    { -- adding morphisms is analogous to &&&
      name = "(" <> name a <> ") &&& (" <> name b <> ")"
    , match = match a :&&: match b
    , contract = contract a >> contract b
    , morphism = morphism a >=> morphism b
    }

seqMorphism :: Enumerable p => Morphism p m -> Morphism p m -> Morphism p m
seqMorphism a b =
  Morphism
    { -- sequencing morphisms is analogous to >>>
      name = "(" <> name a <> ") >>> (" <> name b <> ")"
    , match = match a
    , contract = contract a >> matches (match b) >> contract b
    , morphism = morphism a >=> morphism b
    }
