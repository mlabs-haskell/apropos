{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.Morphism (
  Morphism (..),
  (|:->),
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

(|:->) :: [Morphism p m] -> [Morphism p m] -> [Morphism p m]
(|:->) as bs = [composeMorphisms a b | a <- as, b <- bs]

composeMorphisms :: Morphism p m -> Morphism p m -> Morphism p m
composeMorphisms a b =
  Morphism
    { -- composing morphisms is analogous to >=>
      name = "(" <> name a <> ") >=> (" <> name b <> ")"
    , -- TODO is this correct in the case where contract a changes rather match b matches?
      match = match a :&&: match b
    , contract = contract a >> contract b
    , morphism = morphism a >=> morphism b
    }
