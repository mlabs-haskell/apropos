{-# LANGUAGE RankNTypes #-}
module Brutus.HasPermutationGenerator.PermutationEdge (
  PermutationEdge(..),
  liftEdges,
  composeEdges,
  ) where
import Brutus.LogicalModel.Formula
import Brutus.HasPermutationGenerator.Contract
import Brutus.Gen
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans (lift)
import Data.Maybe (isNothing,catMaybes)

data PermutationEdge p m =
  PermutationEdge {
    name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , permuteGen :: PAGen m m
  }

composeEdges :: PermutationEdge p m -> PermutationEdge p m -> PermutationEdge p m
composeEdges a b =
  PermutationEdge {
    name = name a <> name b
  , match = match a :&&: match b
  , contract = contract a >> contract b
  , permuteGen = do
      m <- lift ask
      ma <- lift $ lift $ runGenPA (permuteGen a) m
      case ma of
        Nothing -> fail "retry"
        Just oh -> do
          mb <- lift $ lift $ runGenPA (permuteGen b) oh
          case mb of
            Nothing -> fail "retry" --TODO remove - I don't think we want retries
            Just so -> pure so
  }

--TODO use the lens library?

liftEdges :: (Ord p, Ord q)
          => (p -> q)       -- The constructor we are lifting into
          -> (m -> n)       -- A lens to extract from the parent model
          -> (n -> m -> m)  -- A lens to insert into the parent model
          -> (q -> Maybe p) -- A lens to extract from the parent property
          -> String         -- A prefix string for the lifted edges
          -> [PermutationEdge p n] -> [PermutationEdge q m]
liftEdges liftProp getSubmodel putSubmodel matchSub prefix edges =
  liftEdge liftProp getSubmodel putSubmodel matchSub prefix <$> edges


liftEdge :: (Ord p, Ord q)
         => (p -> q)       -- The constructor we are lifting into
         -> (m -> n)       -- A lens to extract from the parent model
         -> (n -> m -> m)  -- A lens to insert into the parent model
         -> (q -> Maybe p) -- A lens to extract from the parent property
         -> String         -- A prefix string for the lifted edge
         -> PermutationEdge p n -> PermutationEdge q m
liftEdge liftProp getSubmodel putSubmodel matchSub prefix edge =
  PermutationEdge {
    name = (prefix <> name edge)
  , match = liftProp <$> match edge
  --TODO should we make contract a Free Monad AST?
    -- then we could fmap over it to achieve this
  , contract = do
      i <- readContractOutput
      let subm = extractSubmodel matchSub i
          subx = exciseSubmodel matchSub i
          res = runContract (contract edge)
                            (prefix <> name edge)
                            subm
      case res of
        Left err -> lift $ lift $ lift $ Left err
        Right Nothing -> fail "Nothing"
        Right (Just upd) ->
          output (subx `Set.union` (Set.map liftProp upd))
  , permuteGen = do
     m <- lift ask
     let n = getSubmodel m
     nn <- lift $ lift $ runGenPA (permuteGen edge) n
     case nn of
       Nothing -> fail "retry"
       Just so -> pure $ putSubmodel so m
  }

extractSubmodel :: Ord p => (q -> Maybe p) -> Set q -> Set p
extractSubmodel f q = Set.fromList $ catMaybes (f <$> Set.toList q)

exciseSubmodel :: (q -> Maybe p) -> Set q -> Set q
exciseSubmodel f q = Set.filter (isNothing . f) q

instance Eq (PermutationEdge p m) where
  (==) a b = name a == name b

instance Show (PermutationEdge p m) where
  show = name


