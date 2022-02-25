{-# LANGUAGE RankNTypes #-}
module Apropos.HasPermutationGenerator.Abstraction (
  Abstraction(..),
  abstract,
  abstractsProperties,
  (|:->),
  ) where
import Apropos.LogicalModel.Enumerable
import Apropos.LogicalModel.Formula
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.PermutationEdge
import Apropos.Gen
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either (rights)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ask) --TODO refactor gen monad abstraction
import Control.Lens

data Abstraction ap am bp bm =
  Abstraction {
    abstractionName :: String
  , propertyAbstraction :: Prism' bp ap
  , modelAbstraction :: Lens' bm am
  }

abstract :: Enumerable ap
             => Enumerable bp
             => Abstraction ap am bp bm -> PermutationEdge ap am -> PermutationEdge bp bm
abstract abstraction edge =
  PermutationEdge {
    name = (abstractionName abstraction) <> name edge
  , match = ((propertyAbstraction abstraction) #) <$> match edge
  , contract = abstractContract (abstractionName abstraction)
                                (propertyAbstraction abstraction) $ contract edge
  , permuteGen = do
        m <- ask
        let n = m ^. (modelAbstraction abstraction)
        nn <- lift $ runGenPA (permuteGen edge) n
        pure $ (modelAbstraction abstraction) .~ nn $ m
  }

abstractsProperties :: Enumerable a => Enumerable b => (a -> b) -> Prism' b a
abstractsProperties injection = prism' injection (computeProjection injection)
  where
    computeProjection :: Enumerable a => Enumerable b => (a -> b) -> (b -> Maybe a)
    computeProjection f = g
      where g b = lookup b (zip (f <$> enumerated) enumerated)

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
    projectProperties pa s = Set.fromList $ rights ((matching pa) <$> Set.toList s)
    maskProperties :: Prism' b a -> Set b -> Set b
    maskProperties pa = Set.filter (isn't pa)

(|:->) :: [PermutationEdge p m] -> [PermutationEdge p m] -> [PermutationEdge p m]
(|:->) as bs = [composeEdges a b | a <- as, b <- bs]

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


