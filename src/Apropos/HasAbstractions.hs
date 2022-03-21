{-# LANGUAGE AllowAmbiguousTypes #-}

module Apropos.HasAbstractions(
  HasAbstractions (..),
  AbstractionFor (..),
  abstractionLogic,
  abstractionGenerators,
  ) where


import Apropos.HasParameterisedGenerator
    ( HasParameterisedGenerator )
import Apropos.HasPermutationGenerator
    ( Morphism,
      abstract,
      gotoSum,
      Abstraction,
      HasPermutationGenerator(generators) )
import Apropos.HasPermutationGenerator.Abstraction (abstractLogic)
import Apropos.LogicalModel ( Enumerable, Formula(All) )
import Data.Maybe (catMaybes)
import Control.Monad(join)

class HasAbstractions bp bm where
  abstractions :: [AbstractionFor bp bm]

data AbstractionFor bp bm where
  WrapAbs ::
    forall ap am bp bm.
    ( Enumerable ap
    , Enumerable bp
    , HasParameterisedGenerator ap am
    , HasPermutationGenerator ap am
    ) =>
    Abstraction ap am bp bm ->
    AbstractionFor bp bm

abstractionGenerators :: forall bp bm. HasAbstractions bp bm => [Morphism bp bm]
abstractionGenerators =
  let gotos = catMaybes $ (\(WrapAbs a) -> gotoSum a) <$> abstractions @bp @bm
      abstractMorphisms = (\(WrapAbs a) -> abstract a <$> generators) <$> abstractions @bp @bm
   in gotos ++ join abstractMorphisms

abstractionLogic :: forall bm bp. HasAbstractions bp bm => Formula bp
abstractionLogic = All $ (\(WrapAbs a) -> abstractLogic @bp @bm a) <$> (abstractions @bp @bm)

