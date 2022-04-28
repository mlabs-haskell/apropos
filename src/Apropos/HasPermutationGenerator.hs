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
import Apropos.Gen.BacktrackingTraversal
import Apropos.HasLogicalModel
import Apropos.HasPermutationGenerator.Abstraction
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasPermutationGenerator.Morphism
import Apropos.LogicalModel
import Apropos.Type
import Control.Monad (liftM2, unless, void)
import Data.DiGraph (DiGraph, ShortestPathCache, diameter_, distance_, fromEdges, insertVertex, shortestPathCache, shortestPath_)
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
  traversalRetryLimit :: (m :+ p) -> Int
  traversalRetryLimit _ = 100

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
                  , property $ void $ errorHandler =<< forAll (failWithFootnote "no Morphisms defined" :: Gen String)
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
                    [(fromString "Not strongly connected", property $ void $ forAll (abortNotSCC cache :: Gen String))]
                ]
    where
      abortNotSCC graph =
        let (a, b) = findNoPath (Apropos :: m :+ p) graph
         in failWithFootnote $
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
        (Set p -> Traversal p m) ->
        Morphism p m ->
        Group
      testEdge testRequired pem mGen pe =
        Group (fromString (name pe)) $
          addRequiredTest
            testRequired
            [ (edgeTestName f t, runEdgeTest f)
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
          runRequiredTest = property (errorHandler =<< requiredTestGen)
          requiredTestGen =
            forAll $ do
              if isRequired || allowRedundentMorphisms (Apropos :: p :+ m)
                then pure ()
                else
                  failWithFootnote $
                    renderStyle ourStyle $
                      fromString ("Morphism " <> name pe <> " is not required to make graph strongly connected.")
                        $+$ hang "Edge:" 4 (ppDoc $ name pe)
          runEdgeTest f = property $ do
            void $ traversalContainRetry (traversalRetryLimit (Apropos :: m :+ p)) $ Traversal (mGen f) (\_ -> pure [wrapMorphismWithContractCheck pe])

  buildGen :: Gen m -> Set p -> Traversal p m
  buildGen s tp = do
    let pedges = findMorphisms (Apropos :: m :+ p)
        edges = Map.keys pedges
        graph = fromEdges edges
        cache = shortestPathCache graph
        isco = isStronglyConnected cache
        go targetProperties m = do
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
     in Traversal (Source s) (go tp)

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
      -- The assumption being that smaller morphisms are more general
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
    Gen [Morphism p m]
  transformModel !cache pedges m to = do
    let ps = properties m
    unless (satisfiesFormula logic ps) $ do
      failWithFootnote $
        renderStyle ourStyle $
          "Illegal model produced by the base generator:"
            $+$ hang "model  was:" 4 (ppDoc m)
            $+$ hang "props were:" 4 (ppDoc ps)
    pathOptions <- findPathOptions (Apropos :: m :+ p) cache ps to
    sequence $ traversePath pedges pathOptions

  traversePath ::
    Map (Set p, Set p) [Morphism p m] ->
    [(Set p, Set p)] ->
    [Gen (Morphism p m)]
  traversePath edges es = go <$> es
    where
      go :: (Set p, Set p) -> Gen (Morphism p m)
      go h = do
        pe <- case Map.lookup h edges of
          Nothing ->
            failWithFootnote $
              "tried to traverse and edge that doesn't exist from:" ++ show (fst h) ++ " to: " ++ show (snd h)
                ++ "\nThis is likely because you are using hackage digraph rather than the fork which fixes this"
                ++ "\nhttps://github.com/Geometer1729/digraph"
          Just so -> pure so
        wrapMorphismWithContractCheck <$> element pe

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
     in foldr insertVertex (fromEdges edges) scenarios

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
