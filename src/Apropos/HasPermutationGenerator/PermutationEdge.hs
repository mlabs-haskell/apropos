{-# LANGUAGE RankNTypes #-}

module Apropos.HasPermutationGenerator.PermutationEdge (
  PermutationEdge (..),
  (<$$>),
  (<*>>),
) where

import Apropos.Gen
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel.Formula
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ask)

data PermutationEdge p m = PermutationEdge
  { name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , permuteGen :: PAGen m m
  }

(<*>>) :: [PermutationEdge p m] -> [PermutationEdge p m] -> [PermutationEdge p m]
(<*>>) as bs = [composeEdges a b | a <- as, b <- bs]

composeEdges :: PermutationEdge p m -> PermutationEdge p m -> PermutationEdge p m
composeEdges a b =
  PermutationEdge
    { name = name a <> name b
    , match = match a :&&: match b
    , contract = contract a >> contract b
    , permuteGen = do
        m <- ask
        ma <- lift $ runGenPA (permuteGen a) m
        lift $ runGenPA (permuteGen b) ma
    }

(<$$>) ::
  (Ord p, Ord q) =>
  ModelAbstraction q m p n ->
  [PermutationEdge p n] ->
  [PermutationEdge q m]
(<$$>) abstraction edges = liftEdge abstraction <$> edges

liftEdge ::
  (Ord p, Ord q) =>
  ModelAbstraction q m p n ->
  PermutationEdge p n ->
  PermutationEdge q m
liftEdge abstraction edge =
  PermutationEdge
    { name = ((abstractionName abstraction) <> name edge)
    , match = (injectProperty abstraction) <$> match edge
    , contract = propertyAbstraction abstraction $ contract edge
    , permuteGen = do
        m <- ask
        let n = (projectSubmodel abstraction) m
        nn <- lift $ runGenPA (permuteGen edge) n
        pure $ (injectSubmodel abstraction) nn m
    }

instance Eq (PermutationEdge p m) where
  (==) a b = name a == name b

instance Show (PermutationEdge p m) where
  show = name
