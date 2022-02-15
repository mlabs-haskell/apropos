module Proper.PermutingGenerator (
  PermutingGenerator(..),
  PermutationEdge(..),
  isStronglyConnected,
  ) where
import Proper.HasProperties
import Proper.Proposition
import Data.Set (Set)
import Hedgehog (Gen,PropertyT,forAll,(===))
import qualified Hedgehog.Gen as Gen
import Data.Map (Map)
import qualified Data.Map as Map
import SAT.MiniSat (Formula(..))
import Data.Proxy (Proxy(..))
import Data.Graph (Graph)
import Data.Graph (buildG,scc,dfs)
import Data.Tree (Tree(..))
import Data.Maybe (isNothing)

data PermutationEdge m p =
  PermutationEdge {
    name :: String
  , match :: Formula p
  , contract :: Set p -> Set p
  , permuteGen :: m -> Gen m
  }

instance Show (PermutationEdge m p) where
  show = name

class (HasProperties m p, Show m) => PermutingGenerator m p where
  generators :: [PermutationEdge m p]

  buildGen :: forall t . Monad t => Gen m -> Set p -> PropertyT t m
  buildGen g = do
    let pedges = findPermutationEdges (Proxy :: Proxy m) (Proxy :: Proxy p)
        (sn,_) = numberNodes (Proxy :: Proxy m) (Proxy :: Proxy p)
        graph = buildGraph pedges
        isco = isStronglyConnected graph
     in \targetProperties -> do
          m <- forAll g
          isco === True
          transformModel sn pedges graph m targetProperties

  transformModel :: forall t . Monad t
                 => Map (Set p) Int
                 -> Map (Int,Int) [PermutationEdge m p]
                 -> Graph
                 -> m
                 -> Set p
                 -> PropertyT t m
  transformModel nodes edges graph m to = do
    let pathOptions = findPathOptions graph nodes edges (properties m) to
    traversePath pathOptions m

  traversePath :: forall t . Monad t => [[PermutationEdge m p]] -> m -> PropertyT t m
  traversePath [] m = pure m
  traversePath (h:r) m = do
    tr <- forAll $ Gen.element h
    nm <- forAll $ (permuteGen tr) m
    (contract tr) (properties m) === properties nm
    traversePath r nm

  findPathOptions :: Graph
           -> Map (Set p) Int
           -> Map (Int,Int) [PermutationEdge m p]
           -> Set p -> Set p -> [[PermutationEdge m p]]
  findPathOptions graph ns edges from to =
    let fn = lut ns from
        tn = lut ns to
        pa = pairPath $ computeConnectedPath graph fn tn
     in (lut edges) <$> pa

  buildGraph :: Map (Int,Int) [PermutationEdge m p] -> Graph
  buildGraph pedges =
    let edges = Map.keys pedges
        ub = max (maximum (fst <$> edges)) (maximum (snd <$> edges))
        lb = min (minimum (fst <$> edges)) (minimum (snd <$> edges))
     in buildG (lb,ub) edges

  mapsBetween :: Map Int (Set p) -> Int -> Int -> PermutationEdge m p -> Bool
  mapsBetween m a b pedge =
     satisfiesFormula (match pedge) (lut m a)
        && ((contract pedge) (lut m a)) == (lut m b)

  findPermutationEdges :: Proxy m
                       -> Proxy p
                       -> Map (Int,Int) [PermutationEdge m p]
  findPermutationEdges pm pp =
    let nodemap = snd $ numberNodes pm pp
        nodes = Map.keys nodemap
     in Map.fromList [ ((a,b), filter (mapsBetween nodemap a b) generators )
                     | a <- nodes, b <- nodes]
  numberNodes :: Proxy m
              -> Proxy p
              -> (Map (Set p) Int, Map Int (Set p))
  numberNodes _ (Proxy :: Proxy p) =
    let scenarios = enumerateScenariosWhere (logic :: Formula p)
        scennums = Map.fromList $ zip scenarios [0..]
        numsscen = Map.fromList $ zip [0..] scenarios
    in (scennums,numsscen)

pairPath :: [Int] -> [(Int,Int)]
pairPath [] = []
pairPath [_] = []
pairPath (a:b:r) = (a,b):(pairPath (b:r))

isStronglyConnected :: Graph -> Bool
isStronglyConnected g = 1 == length (scc g)

computeConnectedPath :: Graph -> Int -> Int -> [Int]
computeConnectedPath g f t =
  let paths = dfs g [f]
      blah = case paths of
               [p] -> findPathTo [] p
               _ -> Nothing
   in case blah of
        Nothing -> error "this should never happen"
        Just so -> so
  where findPathTo breadcrumbs (Node i _) | t == i = Just $ reverse (i:breadcrumbs)
        findPathTo breadcrumbs (Node i is) =
          case filter (not . isNothing) (findPathTo (i:breadcrumbs) <$> is) of
            ((Just s):_) -> Just s
            _ -> Nothing

lut :: Ord a => Map a b -> a -> b
lut m i = case Map.lookup i m of
           Nothing -> error "this should never happen"
           Just so -> so

