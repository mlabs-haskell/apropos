{-# LANGUAGE AllowAmbiguousTypes #-}

module Apropos.HasAbstractions (
  HasAbstractions (..),
  AbstractionFor (..),
  abstractionLogic,
  abstractionGenerators,
) where

import Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator,
 )
import Apropos.HasPermutationGenerator (
  Abstraction,
  HasPermutationGenerator (generators),
  Morphism,
  abstract,
  gotoSum,
  (|:->),
 )
import Apropos.HasPermutationGenerator.Abstraction (abstractLogic)
import Apropos.LogicalModel (Enumerable, Formula (All))
import Control.Monad (join)

class HasAbstractions p m where
  abstractions :: [AbstractionFor p m]

data AbstractionFor p m where
  WrapAbs ::
    forall ap am bp bm.
    ( Enumerable ap
    , Enumerable bp
    , HasParameterisedGenerator ap am
    , HasPermutationGenerator ap am
    ) =>
    Abstraction ap am bp bm ->
    AbstractionFor bp bm

abstractionGenerators :: forall p m. HasAbstractions p m => [Morphism p m]
abstractionGenerators =
  let gotos = [morphism | WrapAbs abstraction <- abstractions @p @m, Just morphism <- pure $ gotoSum abstraction]
      abstractMorphisms = [abstract abstraction <$> generators | WrapAbs abstraction <- abstractions @p @m]
   in gotos
        ++ join abstractMorphisms
        ++ join
          [ (abstract a1 <$> generators) |:-> (abstract a2 <$> generators)
          | (WrapAbs a1, WrapAbs a2) <- pairsIn $ abstractions @p @m
          ]
  where
    pairsIn :: [a] -> [(a, a)]
    pairsIn [] = []
    pairsIn (x : xs) = [(x, y) | y <- xs] ++ pairsIn xs

abstractionLogic :: forall m p. HasAbstractions p m => Formula p
abstractionLogic = All [abstractLogic @p @m abstraction | WrapAbs abstraction <- abstractions @p @m]
