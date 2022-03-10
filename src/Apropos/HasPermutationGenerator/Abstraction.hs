{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.Abstraction (
  Abstraction (..),
  abstract,
  abstractsProperties,
) where

import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.Morphism
import Apropos.LogicalModel.Enumerable
import Control.Lens
import Data.Either (rights)
import Data.Set (Set)
import Data.Set qualified as Set

data Abstraction ap am bp bm = Abstraction
  { abstractionName :: String
  , propertyAbstraction :: Prism' bp ap
  , modelAbstraction :: Traversal' bm am
  }

abstract ::
  Enumerable ap =>
  Enumerable bp =>
  Abstraction ap am bp bm ->
  Morphism ap am ->
  Morphism bp bm
abstract abstraction edge =
  Morphism
    { name = abstractionName abstraction <> name edge
    , match = (propertyAbstraction abstraction #) <$> match edge
    , contract =
        abstractContract
          (abstractionName abstraction)
          (propertyAbstraction abstraction)
          $ contract edge
    , morphism = modelAbstraction abstraction $ morphism edge
    }

abstractsProperties :: Enumerable a => Enumerable b => (a -> b) -> Prism' b a
abstractsProperties injection = prism' injection (computeProjection injection)
  where
    computeProjection :: Enumerable a => Enumerable b => (a -> b) -> (b -> Maybe a)
    computeProjection f = g
      where
        g b = lookup b (zip (f <$> enumerated) enumerated)

abstractContract :: (Ord a, Ord b) => String -> Prism' b a -> Contract a () -> Contract b ()
abstractContract prefix a c = do
  i <- readContractOutput
  n <- readContractEdgeName
  let subm = projectProperties a i
      subx = maskProperties a i
      res = runContract c (prefix <> n) subm
  case res of
    Left err -> contractError err
    Right Nothing -> terminal
    Right (Just upd) ->
      output (subx `Set.union` injectProperties a upd)
  where
    injectProperties :: Ord b => Prism' b a -> Set a -> Set b
    injectProperties pa = Set.map (pa #)
    projectProperties :: Ord a => Prism' b a -> Set b -> Set a
    projectProperties pa s = Set.fromList $ rights (matching pa <$> Set.toList s)
    maskProperties :: Prism' b a -> Set b -> Set b
    maskProperties pa = Set.filter (isn't pa)
