{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.Morphism (
  Morphism (..),
  (|:->),
) where

import Apropos.Gen
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel.Formula

data Morphism p m = Morphism
  { name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , morphism :: Gen m m
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
    { name = name a <> name b
    , match = match a :&&: match b
    , contract = contract a >> contract b
    , morphism = do
        m <- source
        ma <- liftMorphism (morphism a) m
        liftMorphism (morphism b) ma
    }


