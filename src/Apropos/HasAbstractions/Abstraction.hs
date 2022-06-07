{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Apropos.HasAbstractions.Abstraction (
  SumAbstraction (..),
  ProductAbstraction (..),
  SourceAbstraction (..),
  PAbs (..),
  Constructor,
  abstractLogicProduct,
  abstractLogicSum,
  abstractProd,
  abstractSum,
  abstractsProperties,
  sumSource,
) where

import Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator,
 )
import Apropos.HasPermutationGenerator (
  HasPermutationGenerator,
  Morphism (..),
  Source (..),
 )
import Apropos.HasPermutationGenerator.Contract (
  Contract,
  labelContract,
 )
import Apropos.Logic (Formula (..), Strategy (..))
import Control.Lens (Lens', Prism', prism', review, (#))
import Data.Kind (Type)

data SourceAbstraction (l :: [(Type, Type)]) p m = SourceAbstraction
  { sourceAbsName :: String
  , constructor :: Constructor l m
  , productAbs :: PAbs l p m
  }

type family Constructor (l :: [(Type, Type)]) m :: Type where
  Constructor '[] m = m
  Constructor ('(ap, am) ': l) m = am -> Constructor l m

infixr 9 :&

data PAbs (l :: [(Type, Type)]) p m where
  Nil :: PAbs '[] p m
  (:&) :: (HasParameterisedGenerator ap am, HasPermutationGenerator ap am) => ProductAbstraction ap am p m -> PAbs l p m -> PAbs ('(ap, am) ': l) p m

data ProductAbstraction ap am bp bm = ProductAbstraction
  { abstractionName :: String
  , propertyAbstraction :: Prism' bp ap
  , productModelAbstraction :: Lens' bm am
  }

data SumAbstraction ap am bp bm = SumAbstraction
  { abstractionName :: String
  , propertyAbstraction :: Prism' bp ap
  , sumModelAbstraction :: Prism' bm am
  , propLabel :: bp
  }

abstractProd ::
  Ord bp =>
  ProductAbstraction ap am bp bm ->
  Morphism ap am ->
  Morphism bp bm
abstractProd abstraction@ProductAbstraction {abstractionName = absName, propertyAbstraction = propAbs} edge =
  Morphism
    { name = absName <> " of (" <> name edge <> ")"
    , match = (propAbs #) <$> match edge
    , contract =
        abstractContract
          propAbs
          $ contract edge
    , morphism = productModelAbstraction abstraction $ morphism edge
    }

abstractSum ::
  Ord bp =>
  SumAbstraction ap am bp bm ->
  Morphism ap am ->
  Morphism bp bm
abstractSum abstraction@SumAbstraction {abstractionName = absName, propertyAbstraction = propAbs} edge =
  Morphism
    { name = absName <> " of ( " <> name edge <> " )"
    , match = Var (propLabel abstraction) :&&: ((propAbs #) <$> match edge)
    , contract =
        abstractContract
          propAbs
          $ contract edge
    , morphism = sumModelAbstraction abstraction $ morphism edge
    }

sumSource ::
  forall ap am bp bm.
  SumAbstraction ap am bp bm ->
  Source ap am ->
  Source bp bm
sumSource
  SumAbstraction
    { abstractionName = absName
    , propertyAbstraction = propAbs
    , sumModelAbstraction = mAbs
    , propLabel = l
    }
  Source {sourceName = sname, covers = c, gen = pg} =
    Source
      { sourceName = absName <> " of " <> sname
      , covers = Var l :&&: ((propAbs #) <$> c)
      , gen = (mAbs #) <$> pg
      }

abstractsProperties :: forall a b t. (Ord b, Strategy a t) => (a -> b) -> Prism' b a
abstractsProperties injection = prism' injection (computeProjection injection)
  where
    computeProjection :: (a -> b) -> (b -> Maybe a)
    computeProjection f = g
      where
        g :: b -> Maybe a
        g b = lookup b (zip (f <$> universe) universe)

abstractContract :: Ord b => Prism' b a -> Contract a () -> Contract b ()
abstractContract a = labelContract (review a)

abstractLogicProduct :: forall bp bm ap am. (Strategy ap am, Strategy bp bm) => ProductAbstraction ap am bp bm -> Formula (NativeVariable bp)
abstractLogicProduct ProductAbstraction {propertyAbstraction = propAbs} = toNativeVariable . (propAbs #) <$> logic

abstractLogicSum :: forall bp bm ap am. (Strategy ap am, Strategy bp bm) => SumAbstraction ap am bp bm -> Formula (NativeVariable bp)
abstractLogicSum SumAbstraction {propertyAbstraction = propAbs, propLabel = sumLabel} =
  toNativeVariable <$>
    (Var sumLabel :->: (propAbs #) <$> logic)
      :&&: (Not (Var sumLabel) :->: None (Var . (propAbs #) <$> universe))
