module Apropos.HasPermutationGenerator (
  HasPermutationGenerator (..),
  Source (..),
  Morphism (..),
  (&&&),
  (>>>),
) where

import Apropos.Error
import Apropos.Gen (
  Gen,
  choice,
  element,
  errorHandler,
  failWithFootnote,
  forAll,
  forAllWithRetries,
  runGenModifiable,
 )
import Apropos.Gen.BacktrackingTraversal (
  Traversal (FromSource, Traversal),
  traversalInGen,
 )
import Apropos.HasLogicalModel (HasLogicalModel (properties, satisfiesExpression))
import Apropos.HasPermutationGenerator.Contract (
  matches,
  solveContract,
 )
import Apropos.HasPermutationGenerator.Morphism (
  Morphism (..),
  addPropCheck,
  (&&&),
  (>>>),
 )
import Apropos.HasPermutationGenerator.Source (
  Source (..),
  wrapSourceWithCheck,
 )
import Apropos.LogicalModel (
  Formula (..),
  LogicalModel (logic, scenarios),
  enumerated,
  solveAll,
 )
import Control.Monad (guard, unless, void, when)
import Data.DiGraph (
  DiGraph,
  ShortestPathCache,
  distance_,
  fromEdges,
  insertVertex,
  shortestPathCache,
  shortestPath_,
  union,
  unsafeFromList,
 )
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, PropertyT, property)
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

  traversalRetryLimit :: Int
  traversalRetryLimit = 100

  allowRedundentMorphisms :: Bool
  allowRedundentMorphisms = False

  permutationValidity :: Maybe (String, PropertyT IO ())
  permutationValidity =
    let pedges = findMorphisms @p
        edges = Map.keys pedges
        graph = unsafeFromList ((,[]) <$> scenarios) `union` fromEdges edges
        sourceMap = findSources @p
        munreachable = unreachableNode (Map.keys sourceMap) cache
        cache = shortestPathCache graph
     in case (munreachable, unconectedSource @p) of
          (Just unreachable, _) ->
            Just
              ( "reachability test"
              , errorHandler =<< runGenModifiable (forAll $ failUnreachable unreachable)
              )
          (Nothing, Just c@(s, _, _)) ->
            Just
              ( fromString $ "connectedness of " ++ sourceName s
              , (errorHandler =<<) $
                runGenModifiable $
                  forAll $
                    failUnconected c
              )
          (Nothing, Nothing) -> Nothing

  permutationGeneratorSelfTest :: Group
  permutationGeneratorSelfTest =
    case permutationValidity @p of
      Just (label, prop) -> Group "permutationValidity test" [(fromString label, property prop)]
      Nothing ->
        Group "permutationGeneratorSelfTest" $
          [ ( fromString $ name m ++ " on " ++ show (fst e) ++ " -> " ++ show (snd e)
            , testEdge e m
            )
          | (e, ms) <- Map.toList $ findMorphisms @p
          , m <- ms
          ]
            ++ [ ( fromString $ "source " ++ sourceName s
                 , testSource s
                 )
               | s <- sources @p
               ]

  testEdge ::
    (Set p, Set p) ->
    Morphism p m ->
    Property
  testEdge (inprops, outprops) m =
    property $
      errorHandler
        =<< runGenModifiable
          ( forAllWithRetries (traversalRetryLimit @p) $
              buildGen inprops >>= void . morphism (addPropCheck (inprops, outprops) m)
          )

  testSource ::
    Source p m ->
    Property
  testSource source =
    property $
      errorHandler
        =<< runGenModifiable
          ( forAllWithRetries (traversalRetryLimit @p) $ do
              m <- gen source
              unless (satisfiesExpression (covers source) m) $
                failWithFootnote $
                  "source: " ++ sourceName source
                    ++ "\nfailed to satisfy coverage condition"
                    ++ "\nmodel was: "
                    ++ show m
                    ++ "\nprops were: "
                    ++ show (properties @p m)
          )

  buildGen :: Set p -> Gen m
  buildGen ps = do
    let pedges = findMorphisms @p
        edges = Map.keys pedges
        graph = unsafeFromList ((,[]) <$> scenarios) `union` fromEdges edges
        sourceMap = findSources @p
        munreachable = unreachableNode (Map.keys sourceMap) cache
        cache = shortestPathCache graph
        viableSources = filter (\source -> reachable cache source ps) (Map.keys sourceMap)
    case munreachable of
      Nothing -> pure ()
      Just unreachable -> failUnreachable unreachable
    let sourceGen = do
          sourceNode <- element viableSources
          fromMaybe (internalError "lookup failed in sourceMap") (Map.lookup sourceNode sourceMap)
    let morphismGen model = do
          let sourceNode = properties model
              viableStops = [n | n <- scenarios, reachable cache sourceNode n, reachable cache n ps]
          unless (sourceNode `elem` viableSources) $
            case unconectedSource @p of
              Just s -> failUnconected s
              Nothing -> internalError "source was not viable but graph was valid"
          when (null viableStops) $ internalError "no stops were possible"
          stop <- element viableStops
          pathp1 <- maybe (internalError "pathfinding failed pre stop") pure $ shortestPath_ sourceNode stop cache
          pathp2 <- maybe (internalError "pathfinding failed post stop") pure $ shortestPath_ stop ps cache
          let path = pairPath $ sourceNode : pathp1 ++ pathp2
          morphisms <- choseMorphism path
          let withPropChecks = zipWith addPropCheck path morphisms
          pure withPropChecks
    traversalInGen (traversalRetryLimit @p) $ Traversal (FromSource sourceGen) morphismGen

  unconectedSource :: Maybe (Source p m, Set p, Set p)
  unconectedSource = listToMaybe $ do
    let es = Map.keysSet findMorphisms
        graph = unsafeFromList ((,[]) <$> scenarios) `union` fromEdges es
        cache = shortestPathCache graph
    s <- sources @p
    let sols =
          Map.keysSet . Map.filter id
            <$> solveAll
              (logic :&&: covers s :&&: All [Var p :||: Not (Var p) | p <- enumerated])
    when (null sols) $ error $ "source: " ++ sourceName s ++ "was empty"
    (p1, p2) <- zip sols (tail sols ++ [head sols])
    guard $ not $ reachable cache p1 p2
    pure (s, p1, p2)

  choseMorphism ::
    [(Set p, Set p)] ->
    Gen [Morphism p m]
  choseMorphism es = sequence $ go <$> es
    where
      go :: (Set p, Set p) -> Gen (Morphism p m)
      go h = do
        pe <- case Map.lookup h (findMorphisms @p) of
          Nothing ->
            error $
              "tried to traverse and edge that doesn't exist from:" ++ show (fst h) ++ " to: " ++ show (snd h)
                ++ "\nThis is likely because you are using hackage digraph rather than the fork which fixes this"
                ++ "\nhttps://github.com/mlabs-haskell/digraph"
          Just so -> pure so
        element pe

  buildGraph :: Map (Set p, Set p) [Morphism p m] -> DiGraph (Set p)
  buildGraph pedges =
    let edges = Map.keys pedges
     in foldr insertVertex (fromEdges edges) scenarios

  findSources :: Map (Set p) (Gen m)
  findSources =
    -- chose randomly for overlapping sources
    Map.map choice $
      Map.fromListWith
        (<>)
        [ (ps, [g])
        | s <- wrapSourceWithCheck <$> sources @p
        , let g :: Gen m = gen s
        , ps <- Map.keysSet . Map.filter id <$> solveAll (logic :&&: covers s :&&: All [Var p :||: Not (Var p) | p <- enumerated])
        ]

  findMorphisms ::
    (Enumerable p) =>
    Map (Set p, Set p) [Morphism p m]
  findMorphisms =
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

failUnreachable :: Show p => Set p -> Gen ()
failUnreachable unreachable =
  failWithFootnote $
    renderStyle ourStyle $
      "Some nodes not reachable"
        $+$ hang "Could not reach node:" 4 (ppDoc unreachable)

failUnconected :: Show p => (Source p m, Set p, Set p) -> Gen ()
failUnconected (s, n1, n2) =
  failWithFootnote $
    "source is not connected"
      ++ "\nsource: "
      ++ sourceName s
      ++ "\nhad no path"
      ++ "\nfrom: "
      ++ show n1
      ++ "\nto: "
      ++ show n2
