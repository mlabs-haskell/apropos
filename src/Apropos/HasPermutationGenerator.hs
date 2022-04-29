module Apropos.HasPermutationGenerator (
  HasPermutationGenerator (..),
  Source (..),
  Morphism (..),
  (&&&),
  (>>>),
) where

import Apropos.Gen (Gen, choice, element, failWithFootnote, forAll)
import Apropos.HasLogicalModel
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.Morphism
import Apropos.HasPermutationGenerator.Source
import Apropos.LogicalModel
import Apropos.Type
import Data.DiGraph (DiGraph, ShortestPathCache, distance_, fromEdges, insertVertex, shortestPathCache, shortestPath_, union, unsafeFromList)
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, property, (===))
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  renderStyle,
  style,
  ($+$),
 )
import Text.Show.Pretty (ppDoc)

class (Hashable p, HasLogicalModel p m, Show m) => HasPermutationGenerator p m where
  generators :: [Morphism p m]
  generators = []

  sources :: [Source p m]

  traversalRetryLimit :: (m :+ p) -> Int
  traversalRetryLimit _ = 100

  allowRedundentMorphisms :: (p :+ m) -> Bool
  allowRedundentMorphisms = const False

  permutationGeneratorSelfTest :: (m :+ p) -> Group
  permutationGeneratorSelfTest _ =
    -- TODO all nodes reachable test here
    -- TODO test sources as well
    Group "permutationGeneratorSelfTest" $
      [ ( fromString $ name m ++ " on " ++ show (fst e) ++ " -> " ++ show (snd e)
        , testEdge e m
        )
      | (e, ms) <- Map.toList (findMorphisms (Apropos :: m :+ p))
      , m <- ms
      ]

  testEdge ::
    (Set p, Set p) ->
    Morphism p m ->
    Property
  testEdge (inprops, outprops) m =
    property $ do
      (inModel :: m) <- forAll $ buildGen inprops
      (outModel :: m) <- forAll $ morphism m inModel
      (properties outModel :: Set p) === (outprops :: Set p)

  buildGen :: Set p -> Gen m
  buildGen ps = do
    let pedges = findMorphisms (Apropos :: m :+ p)
        edges = Map.keys pedges
        graph = unsafeFromList ((,[]) <$> scenarios) `union` fromEdges edges
        sourceMap = findSources (Apropos :: m :+ p)
        munreachable = unreachableNode (Map.keys sourceMap) cache
        cache = shortestPathCache graph
        viableSources = filter (\source -> reachable cache source ps) (Map.keys sourceMap)
    case munreachable of
      Nothing -> pure ()
      Just unreachable ->
        failWithFootnote $
          renderStyle ourStyle $
            "Some nodes not reachable"
              $+$ hang "Could not reach node:" 4 (ppDoc unreachable)
    sourceNode <- element viableSources
    sourceModel <- fromMaybe (failWithFootnote "internal apropos error, lookup failed in sourceMap") (Map.lookup sourceNode sourceMap)
    let viableStops = [n | n <- scenarios, reachable cache sourceNode n, reachable cache n ps]
    stop <- element viableStops
    pathp1 <- maybe (failWithFootnote "internal apropos error: pathfinding failed pre stop") pure $ shortestPath_ sourceNode stop cache
    pathp2 <- maybe (failWithFootnote "internal apropos error: pathfinding failed post stop") pure $ shortestPath_ stop ps cache
    let path = pairPath $ sourceNode : pathp1 ++ pathp2
    morphisms <- choseMorphism path
    let withPropChecks = zipWith addPropCheck path morphisms
    -- TODO make sure this is doing retries right
    foldl (>>=) (pure sourceModel) (morphism <$> withPropChecks)

  choseMorphism ::
    [(Set p, Set p)] ->
    Gen [Morphism p m]
  choseMorphism es = sequence $ go <$> es
    where
      go :: (Set p, Set p) -> Gen (Morphism p m)
      go h = do
        pe <- case Map.lookup h (findMorphisms (Apropos :: m :+ p)) of
          Nothing ->
            failWithFootnote $
              "tried to traverse and edge that doesn't exist from:" ++ show (fst h) ++ " to: " ++ show (snd h)
                ++ "\nThis is likely because you are using hackage digraph rather than the fork which fixes this"
                ++ "\nhttps://github.com/mlabs-haskell/digraph"
          Just so -> pure so
        element pe

  buildGraph :: Map (Set p, Set p) [Morphism p m] -> DiGraph (Set p)
  buildGraph pedges =
    let edges = Map.keys pedges
     in foldr insertVertex (fromEdges edges) scenarios

  findSources ::
    m :+ p ->
    Map (Set p) (Gen m)
  findSources _ =
    -- chose randomly for overlapping sources
    Map.map choice $
      Map.fromListWith
        (<>)
        [ (ps, [g])
        | s <- wrapSourceWithCheck <$> sources @p @m
        , ps <- Map.keysSet . Map.filter id <$> solveAll (logic :&&: covers s)
        , let g :: Gen m = pgen s ps
        ]

  findMorphisms ::
    m :+ p ->
    Map (Set p, Set p) [Morphism p m]
  findMorphisms _ =
    Map.fromListWith
      (<>)
      [ (e, [m])
      | m <- generators
      , e <- Set.toList $ solveContract (matches (match m) >> contract m)
      ]

pairPath :: [a] -> [(a, a)]
pairPath [] = []
pairPath [_] = []
pairPath (a : b : r) = (a, b) : pairPath (b : r)

unreachableNode :: (Hashable p, LogicalModel p) => [Set p] -> ShortestPathCache (Set p) -> Maybe (Set p)
unreachableNode sourceNodes cache =
  listToMaybe $
    [ps | ps <- scenarios, not $ any (\s -> reachable cache s ps) sourceNodes]

ourStyle :: Style
ourStyle = style {lineLength = 80}

reachable :: (Eq a, Hashable a) => ShortestPathCache a -> a -> a -> Bool
reachable cache s e = isJust $ distance_ s e cache
