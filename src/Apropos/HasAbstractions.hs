{-# LANGUAGE AllowAmbiguousTypes #-}

module Apropos.HasAbstractions (
  HasAbstractions (..),
  AbstractionFor (..),
  abstractionLogic,
  abstractionGenerators,
  parallelAbstractionMorphisms,
) where

import Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator,
 )
import Apropos.HasPermutationGenerator (
  Abstraction(..),
  HasPermutationGenerator (generators),
  Morphism,
  abstract,
  gotoSum,
  (|:->),
 )
import Apropos.HasPermutationGenerator.Abstraction (abstractLogic)
import Apropos.LogicalModel (Enumerable, Formula (All), LogicalModel)
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

abstractionGenerators :: forall p m. (LogicalModel p,HasAbstractions p m) => [Morphism p m]
abstractionGenerators =
  let gotos = [morphism | WrapAbs abstraction <- abstractions @p @m, Just morphism <- pure $ gotoSum abstraction]
      abstractMorphisms = [abstract abstraction <$> generators | WrapAbs abstraction <- abstractions @p @m]
   in gotos
        ++ join abstractMorphisms

parallelAbstractionMorphisms :: forall p m. (LogicalModel p,HasAbstractions p m) => [Morphism p m]
parallelAbstractionMorphisms =
  let abstractProductMorphisms = [abstract abstraction <$> generators | WrapAbs abstraction@ProductAbstraction{} <- abstractions @p @m]
  in join
        [ foldl  (|:->) m ms
        | m:ms@(_:_) <- seqs abstractProductMorphisms ]
 where
   seqs :: [a] -> [[a]]
   seqs [] = [[]]
   seqs (x:xs) = let xs' = seqs xs in xs' ++ ((x:) <$> xs')

abstractionLogic :: forall m p. HasAbstractions p m => Formula p
abstractionLogic = All [abstractLogic @p @m abstraction | WrapAbs abstraction <- abstractions @p @m]
