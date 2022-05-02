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
  Correction (..),
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
  Source (Source, sourceName),
  (&&&),
 )
import Apropos.LogicalModel (Enumerable, LogicalModel)
import Apropos.LogicalModel.Formula
import Control.Lens ((#), (^?))
import Control.Monad (join, (>=>))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

class LogicalModel p => HasAbstractions p m where
  sumAbstractions :: [SumAbstractionFor p m]
  sumAbstractions = []
  productAbstractions :: [ProductAbstractionFor p m]
  productAbstractions = sourceProductAbstractions
  sourceAbstractions :: [SourceAbstractionFor p m]
  sourceAbstractions = []
  sourceCorections :: [Correction p m]
  sourceCorections = []
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

data Correction p m = Correction
  { corName :: String
  , domain :: Formula p
  , modifier :: m -> Gen m
  }

abstractionMorphisms :: forall p m. (LogicalModel p, HasAbstractions p m) => [Morphism p m]
abstractionMorphisms =
  let productAbstractionMorphisms = join [abstractProd abstraction <$> generators | PAs abstraction <- productAbstractions @p @m]
      sumAbstractionMorphism = join [abstractSum abstraction <$> generators | SuAs abstraction <- sumAbstractions @p @m]
   in productAbstractionMorphisms ++ sumAbstractionMorphism

abstractionSources :: forall p m. HasAbstractions p m => [Source p m]
abstractionSources =
  let corectedSources = [Source (sn ++ "fixed by " ++ cn) (sc :&&: cd) (pg >=> m) | Source sn sc pg <- abstractionSources', Correction cn cd m <- sourceCorections]
      uncorectedLogic = All [Not cd | Correction _ cd _ <- sourceCorections @p @m]
      uncorected = [Source sn (sc :&&: uncorectedLogic) pg | Source sn sc pg <- abstractionSources']
   in corectedSources ++ uncorected

abstractionSources' :: forall p m. HasAbstractions p m => [Source p m]
abstractionSources' = sourcesFromSourceAbstractions ++ [sumSource sa s | SuAs sa <- sumAbstractions, s <- sources]

{- | Product types with additional logic sometimes need to include parallel morphisms
 which change both fields of the product to keep some invariant
-}
parallelAbstractionMorphisms :: forall p m. HasAbstractions p m => [Morphism p m]
parallelAbstractionMorphisms =
  let abstractProductMorphisms = [abstractProd abstraction <$> generators | PAs abstraction <- productAbstractions @p @m]
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
  All [abstractLogicProduct @p @m abstraction | PAs abstraction <- productAbstractions @p @m]
    :&&: All [abstractLogicSum @p @m abstraction | SuAs abstraction <- sumAbstractions @p @m]

sourcesFromSourceAbstractions :: HasAbstractions p m => [Source p m]
sourcesFromSourceAbstractions = join [sourcesFromAbstraction a | SoAs a <- sourceAbstractions]

sourcesFromAbstraction :: SourceAbstraction l p m -> [Source p m]
sourcesFromAbstraction (SourceAbstraction sname con pabs) = do
  s <- withSources (const $ pure con) pabs
  pure $ s {sourceName = sname ++ "over" ++ "[" ++ sourceName s ++ "]"}

withSources :: (Set p -> Gen (Constructor l m)) -> PAbs l p m -> [Source p m]
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
      (Source sName sLogic sPGen) <- sources @ap @am
      let c' =
            ( \props ->
                c props
                  <*> sPGen (Set.fromList . mapMaybe (^? pabs) . Set.toList $ props)
            )
      (Source sName' sLogic' sPGen') <- withSources c' ps
      pure $
        Source
          (pabsname ++ " of " ++ sName ++ "," ++ sName')
          (((pabs #) <$> sLogic) :&&: sLogic')
          sPGen'
