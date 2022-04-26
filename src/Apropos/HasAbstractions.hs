{-# LANGUAGE AllowAmbiguousTypes #-}

module Apropos.HasAbstractions (
  HasAbstractions (..),
  AbstractionFor (..),
  abstractionLogic,
  abstractionMorphisms,
  parallelAbstractionMorphisms,
) where

import Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator,
 )
import Apropos.HasPermutationGenerator (
  Abstraction (..),
  HasPermutationGenerator (generators),
  Morphism,
  abstract,
  gotoSum,
  (&&&),
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

abstractionMorphisms :: forall p m. (HasAbstractions p m) => [Morphism p m]
abstractionMorphisms =
  let gotos = [morphism | WrapAbs abstraction <- abstractions @p @m, Just morphism <- pure $ gotoSum abstraction]
      abstractMorphisms = [abstract abstraction <$> generators | WrapAbs abstraction <- abstractions @p @m]
   in gotos
        ++ join abstractMorphisms

{- | Product types with additional logic sometimes need to include parallel morphisms
 which change both fields of the product to keep some invariant
-}
parallelAbstractionMorphisms :: forall p m. (HasAbstractions p m) => [Morphism p m]
parallelAbstractionMorphisms =
  let abstractProductMorphisms = [abstract abstraction <$> generators | WrapAbs abstraction@ProductAbstraction {} <- abstractions @p @m]
   in join
        [ foldl (&&&) m ms
        | m : ms@(_ : _) <- seqs abstractProductMorphisms
        ]
  where
    seqs :: [a] -> [[a]]
    seqs [] = [[]]
    seqs (x : xs) = let xs' = seqs xs in xs' ++ ((x :) <$> xs')

abstractionLogic :: forall m p. HasAbstractions p m => Formula p
abstractionLogic = All [abstractLogic @p @m abstraction | WrapAbs abstraction <- abstractions @p @m]
