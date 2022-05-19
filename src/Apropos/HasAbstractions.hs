{-# LANGUAGE AllowAmbiguousTypes #-}

module Apropos.HasAbstractions (
  HasAbstractions (..),
  ProductAbstraction (..),
  SumAbstraction (..),
  SourceAbstraction (..),
  SumAbstractionFor (..),
  ProductAbstractionFor (..),
  SourceAbstractionFor (..),
  PAbs (..),
  abstractionLogic,
  abstractionMorphisms,
  abstractionSources,
  parallelAbstractionMorphisms,
  abstractsProperties,
) where

import Apropos.Gen
import Apropos.HasAbstractions.Abstraction (
  Constructor,
  PAbs (..),
  ProductAbstraction (..),
  SourceAbstraction (..),
  SumAbstraction (..),
  abstractLogicProduct,
  abstractLogicSum,
  abstractProd,
  abstractSum,
  abstractsProperties,
  sumSource,
 )
import Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator,
 )
import Apropos.HasPermutationGenerator (
  HasPermutationGenerator (generators, sources),
  Morphism,
  Source (Source, covers, sourceName),
  (&&&),
 )
import Apropos.LogicalModel (Enumerable, LogicalModel (logic))
import Apropos.LogicalModel.Formula
import Control.Lens ((#))
import Control.Monad (guard, join)

class LogicalModel p => HasAbstractions p m | p -> m where
  sumAbstractions :: [SumAbstractionFor p m]
  sumAbstractions = []
  productAbstractions :: [ProductAbstractionFor p m]
  productAbstractions = sourceProductAbstractions
  sourceAbstractions :: [SourceAbstractionFor p m]
  sourceAbstractions = []
  {-# MINIMAL sumAbstractions | productAbstractions | sourceAbstractions #-}

sourceProductAbstractions :: forall p m. (HasAbstractions p m) => [ProductAbstractionFor p m]
sourceProductAbstractions = join [productAbstractionsIn l | SoAs SourceAbstraction {productAbs = l} <- sourceAbstractions]
  where
    productAbstractionsIn :: PAbs l p m -> [ProductAbstractionFor p m]
    productAbstractionsIn Nil = []
    productAbstractionsIn ((p :: ProductAbstraction ap am p m) :& ps) = PAs p : productAbstractionsIn ps

data SourceAbstractionFor p m where
  SoAs ::
    forall p m l.
    SourceAbstraction l p m ->
    SourceAbstractionFor p m

data ProductAbstractionFor p m where
  PAs ::
    forall ap am bp bm.
    ( Enumerable ap
    , Enumerable bp
    , HasParameterisedGenerator ap am
    , HasPermutationGenerator ap am
    ) =>
    ProductAbstraction ap am bp bm ->
    ProductAbstractionFor bp bm

data SumAbstractionFor p m where
  SuAs ::
    forall ap am bp bm.
    ( Enumerable ap
    , Enumerable bp
    , HasParameterisedGenerator ap am
    , HasPermutationGenerator ap am
    ) =>
    SumAbstraction ap am bp bm ->
    SumAbstractionFor bp bm

abstractionMorphisms :: forall p m. (HasAbstractions p m) => [Morphism p m]
abstractionMorphisms =
  let productAbstractionMorphisms = join [abstractProd abstraction <$> generators | PAs abstraction <- productAbstractions @p]
      sumAbstractionMorphism = join [abstractSum abstraction <$> generators | SuAs abstraction <- sumAbstractions @p]
   in productAbstractionMorphisms ++ sumAbstractionMorphism

abstractionSources :: forall p m. HasAbstractions p m => [Source p m]
abstractionSources = sourcesFromSourceAbstractions ++ [sumSource sa s | SuAs sa <- sumAbstractions, s <- sources]

{- | Product types with additional logic sometimes need to include parallel morphisms
 which change both fields of the product to keep some invariant
-}
parallelAbstractionMorphisms :: forall p m. (HasAbstractions p m) => [Morphism p m]
parallelAbstractionMorphisms =
  let abstractProductMorphisms = [abstractProd abstraction <$> generators | PAs abstraction <- productAbstractions @p]
   in join
        [ foldl (&&&) m ms
        | m : ms@(_ : _) <- seqs abstractProductMorphisms
        ]
  where
    seqs :: [a] -> [[a]]
    seqs [] = [[]]
    seqs (x : xs) = let xs' = seqs xs in xs' ++ ((x :) <$> xs')

abstractionLogic :: forall m p. HasAbstractions p m => Formula p
abstractionLogic =
  All [abstractLogicProduct @p abstraction | PAs abstraction <- productAbstractions @p]
    :&&: All [abstractLogicSum @p abstraction | SuAs abstraction <- sumAbstractions @p]

sourcesFromSourceAbstractions :: HasAbstractions p m => [Source p m]
sourcesFromSourceAbstractions = join [sourcesFromAbstraction a | SoAs a <- sourceAbstractions]

sourcesFromAbstraction :: LogicalModel p => SourceAbstraction l p m -> [Source p m]
sourcesFromAbstraction (SourceAbstraction sname con pabs) = do
  s <- withSources (pure con) pabs
  guard $ satisfiable (logic :&&: covers s) -- remove sources which can never be used
  pure $ s {sourceName = sname ++ " over [" ++ init (sourceName s) ++ "]"} -- init drops a trailing ,

withSources :: Gen (Constructor l m) -> PAbs l p m -> [Source p m]
withSources c Nil = pure $ Source "" Yes c
withSources
  c
  ( ( ProductAbstraction
        { propertyAbstraction = pabs
        , abstractionName = pabsname
        } ::
        ProductAbstraction ap am p m
      )
      :& ps
    ) =
    do
      (Source sName sLogic sGen) <- sources @ap @am
      let c' = c <*> sGen
      (Source sName' sLogic' sPGen') <- withSources c' ps
      pure $
        Source
          (pabsname ++ " of " ++ sName ++ "," ++ sName')
          (((pabs #) <$> sLogic) :&&: sLogic')
          sPGen'
