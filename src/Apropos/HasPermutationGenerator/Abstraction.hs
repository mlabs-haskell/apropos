{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.Abstraction (
  Abstraction (..),
  abstract,
  gotoSum,
  abstractLogic,
  abstractsProperties,
) where

import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.Morphism
import Apropos.LogicalModel (Formula (..), LogicalModel (logic), satisfiedBy)
import Apropos.LogicalModel.Enumerable
import Control.Lens

data Abstraction ap am bp bm
  = ProductAbstraction
      { abstractionName :: String
      , propertyAbstraction :: Prism' bp ap
      , productModelAbstraction :: Lens' bm am
      }
  | SumAbstraction
      { abstractionName :: String
      , propertyAbstraction :: Prism' bp ap
      , sumModelAbstraction :: Prism' bm am
      , propLabel :: bp
      }

abstract ::
  Abstraction ap am bp bm ->
  Morphism ap am ->
  Morphism bp bm
abstract abstraction@ProductAbstraction {} edge =
  Morphism
    { name = abstractionName abstraction <> " of (" <> name edge <> ")"
    , match = (propertyAbstraction abstraction #) <$> match edge
    , contract =
        abstractContract
          (propertyAbstraction abstraction)
          $ contract edge
    , morphism = productModelAbstraction abstraction $ morphism edge
    }
abstract abstraction@SumAbstraction {} edge =
  Morphism
    { name = abstractionName abstraction <> " of ( " <> name edge <> " )"
    , match = Var (propLabel abstraction) :&&: ((propertyAbstraction abstraction #) <$> match edge)
    , contract =
        abstractContract
          (propertyAbstraction abstraction)
          $ contract edge
    , morphism = sumModelAbstraction abstraction $ morphism edge
    }

gotoSum ::
  forall ap am bp bm.
  Enumerable bp =>
  HasParameterisedGenerator ap am =>
  Abstraction ap am bp bm ->
  Maybe (Morphism bp bm)
gotoSum ProductAbstraction {} = Nothing
gotoSum s@SumAbstraction {} =
  Just $
    Morphism
      { name = "goto " <> abstractionName s
      , match = Not $ Var $ propLabel s
      , contract = clear >> add (propLabel s) >> addAll ((propertyAbstraction s #) <$> satisfiedBy)
      , morphism =
          const $
            (sumModelAbstraction s #)
              <$> genSatisfying @ap @am (All $ Var <$> satisfiedBy)
      }

abstractsProperties :: Enumerable a => Enumerable b => (a -> b) -> Prism' b a
abstractsProperties injection = prism' injection (computeProjection injection)
  where
    computeProjection :: Enumerable a => Enumerable b => (a -> b) -> (b -> Maybe a)
    computeProjection f = g
      where
        g b = lookup b (zip (f <$> enumerated) enumerated)

abstractContract :: Prism' b a -> Contract a () -> Contract b ()
abstractContract a = labelContract (review a)

abstractLogic :: forall bp bm ap am. LogicalModel ap => Abstraction ap am bp bm -> Formula bp
abstractLogic s@ProductAbstraction {} = (propertyAbstraction s #) <$> logic
abstractLogic s@SumAbstraction {} =
  (Var (propLabel s) :->: (propertyAbstraction s #) <$> logic)
    :&&: (Not (Var (propLabel s)) :->: None (Var . (propertyAbstraction s #) <$> enumerated))
