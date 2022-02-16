module Proper.PermutingGenerator (
  PermutingGenerator(..),
  PermutationEdge(..),
  isStronglyConnected,
  ) where

import Proper.HasProperties
import Proper.Proposition
import Data.Set (Set)
import Hedgehog (Gen,PropertyT,MonadTest,forAll,failure,footnote)
import qualified Hedgehog.Gen as Gen
import Data.Map (Map)
import qualified Data.Map as Map
import SAT.MiniSat (Formula(..))
import Data.Proxy (Proxy(..))
import Data.Graph (Graph)
import Data.Graph (buildG,scc,dfs,path)
import Data.Tree (Tree(..))
import Data.Maybe (isNothing)
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  renderStyle,
  style,
  ($+$),
 )


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
        (sn,ns) = numberNodes (Proxy :: Proxy m) (Proxy :: Proxy p)
        graph = buildGraph pedges
        isco = isStronglyConnected graph
     in \targetProperties -> do
          m <- forAll g
          if length pedges == 0
             then failWithFootnote "no PermutationEdges defined"
             else pure ()
          if isco
             then pure ()
             else
               let (a,b) = findNoPath (Proxy :: Proxy m) ns graph
                in failWithFootnote $ renderStyle ourStyle $
                      "PermutationEdges do not form a strongly connected graph."
                      $+$ hang "No Edge Between here:" 4 (ppDoc a)
                      $+$ hang "            and here:" 4 (ppDoc b)
          transformModel sn pedges graph m targetProperties

  findNoPath :: Proxy m
             -> Map Int (Set p)
             -> Graph
             -> (Set p, Set p)
  findNoPath _ m g = head [ (lut m a, lut m b)
                          | a <- Map.keys m
                          , b <- Map.keys m
                          , not (path g a b)
                          ]

  transformModel :: forall t . Monad t
                 => Map (Set p) Int
                 -> Map (Int,Int) [PermutationEdge m p]
                 -> Graph
                 -> m
                 -> Set p
                 -> PropertyT t m
  transformModel nodes edges graph m to = do
    pathOptions <- findPathOptions (Proxy :: Proxy m) graph nodes (properties m) to
    traversePath edges pathOptions m

  traversePath :: forall t . Monad t => Map (Int,Int) [PermutationEdge m p]
                 -> [(Int,Int)] -> m -> PropertyT t m
  traversePath _ [] m = pure m
  traversePath edges (h:r) m = do
    pe <- case Map.lookup h edges of
            Nothing -> failWithFootnote "this should never happen"
            Just so -> pure so
    tr <- forAll $ Gen.element pe
    nm <- forAll $ (permuteGen tr) m
    let expected = (contract tr) (properties m)
        observed = properties nm
    if expected == observed
      then pure ()
      else failWithFootnote $ renderStyle ourStyle $
             "PermutationEdge fails its contract."
               $+$ hang "Edge:" 4 (ppDoc $ name tr)
               $+$ hang "Expected:" 4 (ppDoc expected)
               $+$ hang "Observed:" 4 (ppDoc observed)
    traversePath edges r nm

  findPathOptions ::  forall t . Monad t => (Proxy m)
                  -> Graph
                  -> Map (Set p) Int
                  -> Set p -> Set p -> PropertyT t [(Int,Int)]
  findPathOptions _ graph ns from to = do
    fn <- case Map.lookup from ns of
            Nothing -> failWithFootnote $ renderStyle ourStyle $
                        "Model logic inconsistency?"
                         $+$ hang "Not in graph:" 4 (ppDoc from)
            Just so -> pure so
    tn <- case Map.lookup to ns of
            Nothing -> failWithFootnote "to node not found"
            Just so -> pure so
    pure $ pairPath $ computeConnectedPath graph fn tn

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
     in Map.fromList [ ((a,b), options )
                     | a <- nodes
                     , b <- nodes
                     , let options = filter (mapsBetween nodemap a b) generators
                     , length options > 0 ]
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

failWithFootnote :: MonadTest m => String -> m a
failWithFootnote s = footnote s >> failure

ourStyle :: Style
ourStyle = style {lineLength = 80}

