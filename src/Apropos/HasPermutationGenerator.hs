module Apropos.HasPermutationGenerator (
  HasPermutationGenerator (..),
  Morphism (..),
  Abstraction (..),
  abstract,
  gotoSum,
  abstractsProperties,
  (&&&),
  (>>>),
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasPermutationGenerator.Abstraction
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.Morphism
import Apropos.LogicalModel
import Apropos.Type
import Control.Monad (liftM2)
import Data.DiGraph (DiGraph, ShortestPathCache, diameter_, distance_, fromEdges, shortestPathCache, shortestPath_)
import Data.Function (on)
import Data.Hashable (Hashable)
import Data.List (minimumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), failure, property)
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

  allowRedundentMorphisms :: (p :+ m) -> Bool
  allowRedundentMorphisms = const False

  permutationGeneratorSelfTest :: Bool -> (Morphism p m -> Bool) -> Gen m -> [Group]
  permutationGeneratorSelfTest testForSuperfluousEdges pefilter bgen =
    let pedges = findMorphisms (Apropos :: m :+ p)
        graph = buildGraph pedges
        cache = shortestPathCache graph
        mGen = buildGen bgen
        isco = isStronglyConnected cache
     in if null (Map.keys pedges)
          then
            [ Group
                "No permutation edges defined."
                [
                  ( fromString "no edges defined"
                  , genProp $ failWithFootnote "no Morphisms defined"
                  )
                ]
            ]
          else
            if isco
              then case findDupEdgeNames of
                [] ->
                  testEdge testForSuperfluousEdges pedges mGen
                    <$> filter pefilter generators
                dups ->
                  [ Group "HasPermutationGenerator edge names must be unique." $
                      [ (fromString $ dup <> " not unique", property failure)
                      | dup <- dups
                      ]
                  ]
              else
                [ Group
                    "HasPermutationGenerator Graph Not Strongly Connected"
                    [(fromString "Not strongly connected", abortNotSCC cache)]
                ]
    where
      abortNotSCC graph =
        let (a, b) = findNoPath (Apropos :: m :+ p) graph
         in genProp $
              failWithFootnote $
                renderStyle ourStyle $
                  "Morphisms do not form a strongly connected graph."
                    $+$ hang "No Edge Between here:" 4 (ppDoc a)
                    $+$ hang "            and here:" 4 (ppDoc b)
      findDupEdgeNames =
        [ name g | g <- generators :: [Morphism p m], length (filter (== g) generators) > 1
        ]
      testEdge ::
        Bool ->
        Map (Set p, Set p) [Morphism p m] ->
        (Set p -> Gen m) ->
        Morphism p m ->
        Group
      testEdge testRequired pem mGen pe =
        Group (fromString (name pe)) $
          addRequiredTest
            testRequired
            [ (edgeTestName f t, runEdgeTest f t)
            | (f, t) <- matchesEdges
            ]
        where
          addRequiredTest False l = l
          addRequiredTest True l = (fromString "Is Required", runRequiredTest) : l
          matchesEdges = [e | (e, v) <- Map.toList pem, pe `elem` v]
          edgeTestName f t = fromString $ name pe <> " : " <> show (Set.toList f) <> " -> " <> show (Set.toList t)
          isRequired =
            let inEdges = [length v | (_, v) <- Map.toList pem, pe `elem` v]
             in elem 1 inEdges
          runRequiredTest = genProp $ do
            if isRequired || allowRedundentMorphisms (Apropos :: p :+ m)
              then pure ()
              else
                failWithFootnote $
                  renderStyle ourStyle $
                    fromString ("Morphism " <> name pe <> " is not required to make graph strongly connected.")
                      $+$ hang "Edge:" 4 (ppDoc $ name pe)
          runEdgeTest f t = genProp $ do
            om <- mGen f
            nm <- morphism pe om
            let expected = t
                observed = properties nm
            if expected == observed
              then pure ()
              else edgeFailsContract pe om nm expected observed

  buildGen :: Gen m -> Set p -> Gen m
  buildGen g = do
    let pedges = findMorphisms (Apropos :: m :+ p)
        edges = Map.keys pedges
        graph = fromEdges edges
        cache = shortestPathCache graph
        isco = isStronglyConnected cache
        go targetProperties = do
          m <- g
          if null pedges
            then failWithFootnote "no Morphisms defined"
            else pure ()
          if isco
            then pure ()
            else
              let (a, b) = findNoPath (Apropos :: m :+ p) cache
               in failWithFootnote $
                    renderStyle ourStyle $
                      "Morphisms do not form a strongly connected graph."
                        $+$ hang "No Edge Between here:" 4 (ppDoc a)
                        $+$ hang "            and here:" 4 (ppDoc b)
          transformModel cache pedges m targetProperties
     in go

  findNoPath ::
    m :+ p ->
    ShortestPathCache (Set p) ->
    (Set p, Set p)
  findNoPath _ !cache =
    minimumBy
      (compare `on` uncurry score)
      [ (a, b)
      | a <- scenarios
      , b <- scenarios
      , isNothing (distance_ a b cache)
      ]
    where
      -- The score function is designed to favor sets which are similar and small
      -- The assumption being that smaller morphims are more general
      score :: Ord a => Set a -> Set a -> (Int, Int)
      score l r = (hamming l r, length $ l `Set.intersection` r)
      hamming :: Ord a => Set a -> Set a -> Int
      hamming l r = length (l `setXor` r)
      setXor :: Ord a => Set a -> Set a -> Set a
      setXor l r = (l `Set.difference` r) `Set.union` (r `Set.difference` l)

  transformModel ::
    ShortestPathCache (Set p) ->
    Map (Set p, Set p) [Morphism p m] ->
    m ->
    Set p ->
    Gen m
  transformModel !cache pedges m to = do
    pathOptions <- findPathOptions (Apropos :: m :+ p) cache (properties m) to
    traversePath pedges pathOptions m

  traversePath ::
    Map (Set p, Set p) [Morphism p m] ->
    [(Set p, Set p)] ->
    m ->
    Gen m
  traversePath _ [] m = pure m
  traversePath edges (h : r) m = do
    pe <- case Map.lookup h edges of
      Nothing -> failWithFootnote "tried to travel edge with no morphism"
      Just so -> pure so
    tr <- element pe
    let inprops = properties m
        mexpected = runContract (contract tr) inprops
    -- TODO this probably needs to be cached somehow
    case mexpected of
      Nothing ->
        failWithFootnote $
          renderStyle ourStyle $
            "Morphism doesn't work. This is a model error"
              $+$ "This should never happen at this point in the program."
      Just expected -> do
        if satisfiesFormula logic expected
          then pure ()
          else
            failWithFootnote $
              renderStyle ourStyle $
                "Morphism contract produces invalid model"
                  $+$ hang "Edge:" 4 (ppDoc $ name tr)
                  $+$ hang "Input:" 4 (ppDoc inprops)
                  $+$ hang "Output:" 4 (ppDoc expected)
        label $ fromString $ name tr
        nm <- morphism tr m
        let observed = properties nm
        if expected == observed
          then pure ()
          else edgeFailsContract tr m nm expected observed
        traversePath edges r nm

  findPathOptions ::
    m :+ p ->
    ShortestPathCache (Set p) ->
    Set p ->
    Set p ->
    Gen [(Set p, Set p)]
  findPathOptions _ !cache from to = do
    pairPath <$> genRandomPath cache from to

  buildGraph :: Map (Set p, Set p) [Morphism p m] -> DiGraph (Set p)
  buildGraph pedges =
    let edges = Map.keys pedges
     in fromEdges edges

  findMorphisms ::
    m :+ p ->
    Map (Set p, Set p) [Morphism p m]
  findMorphisms _ =
    Map.fromListWith
      (<>)
      [ (e, [m])
      | m <- generators
      , e <- Set.toList $ solveContract (contract m)
      ]

pairPath :: [a] -> [(a, a)]
pairPath [] = []
pairPath [_] = []
pairPath (a : b : r) = (a, b) : pairPath (b : r)

isStronglyConnected :: ShortestPathCache a -> Bool
isStronglyConnected !cache = isJust $ diameter_ cache

ourStyle :: Style
ourStyle = style {lineLength = 80}

genRandomPath :: (LogicalModel p, Hashable p) => ShortestPathCache (Set p) -> Set p -> Set p -> Gen [Set p]
genRandomPath !cache from to = do
  mid <- element scenarios
  let p1 = shortestPath_ from mid cache
  let p2 = shortestPath_ mid to cache
   in case liftM2 (<>) p1 p2 of
        Just p -> pure $ from : p
        Nothing -> error "failed to find path despite graph being connected?"

edgeFailsContract ::
  forall m p.
  HasLogicalModel p m =>
  Show m =>
  Morphism p m ->
  m ->
  m ->
  Set p ->
  Set p ->
  Gen ()
edgeFailsContract tr m nm expected observed =
  failWithFootnote $
    renderStyle ourStyle $
      "Morphism fails its contract."
        $+$ hang "Edge:" 4 (ppDoc $ name tr)
        $+$ hang "InputModel:" 4 (ppDoc (ppDoc m))
        $+$ hang "InputProperties" 4 (ppDoc $ Set.toList (properties m :: Set p))
        $+$ hang "OutputModel:" 4 (ppDoc (ppDoc nm))
        $+$ hang "ExpectedProperties:" 4 (ppDoc (Set.toList expected))
        $+$ hang "ObservedProperties:" 4 (ppDoc (Set.toList observed))
