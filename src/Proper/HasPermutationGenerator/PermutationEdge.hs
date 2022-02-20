{-# LANGUAGE RankNTypes #-}
module Proper.HasPermutationGenerator.PermutationEdge (
  PermutationEdge(..),
  liftEdges,
  ) where
import Proper.LogicalModel.Formula
import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Reader (ask,runReaderT)
import Control.Monad.Trans (lift)
import Data.Maybe (isNothing,catMaybes)

data PermutationEdge p m =
  PermutationEdge {
    name :: String
  , match :: Formula p
  , contract :: Contract p ()
  , permuteGen :: PAGen m m
  }

liftEdges :: (Ord p,Ord q) => (p -> q) -> (m -> n) -> (n -> m -> m) -> (q -> Maybe p)
          -> String -> [PermutationEdge p n] -> [PermutationEdge q m]
liftEdges liftProp getSubmodel putSubmodel matchSub prefix edges =
  liftEdge liftProp getSubmodel putSubmodel matchSub prefix <$> edges


liftEdge :: (Ord p, Ord q) => (p -> q) -> (m -> n) -> (n -> m -> m) -> (q -> Maybe p)
         -> String -> PermutationEdge p n -> PermutationEdge q m
liftEdge liftProp getSubmodel putSubmodel matchSub prefix edge =
  PermutationEdge {
    name = (prefix <> name edge)
  , match = liftProp <$> match edge
  , contract = do
      i <- readContractInput
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
     m <- ask
     let n = getSubmodel m
     nn <- lift $ runReaderT (permuteGen edge) n
     pure $ putSubmodel nn m
  }

extractSubmodel :: Ord p => (q -> Maybe p) -> Set q -> Set p
extractSubmodel f q = Set.fromList $ catMaybes (f <$> Set.toList q)

exciseSubmodel :: (q -> Maybe p) -> Set q -> Set q
exciseSubmodel f q = Set.filter (isNothing . f) q

instance Eq (PermutationEdge p m) where
  (==) a b = name a == name b

instance Show (PermutationEdge p m) where
  show = name


