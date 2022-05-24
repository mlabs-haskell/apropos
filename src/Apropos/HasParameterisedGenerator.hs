module Apropos.HasParameterisedGenerator (
  HasParameterisedGenerator (..),
  runGeneratorTest,
  runGeneratorTestsWhere,
  enumerateGeneratorTest,
  enumerateGeneratorTestsWhere,
  genSatisfying,
  sampleGenTest,
) where

import Apropos.Gen
import Apropos.Gen.Enumerate (enumerate)
import Apropos.Logic (
  Formula,
  Strategy (variablesSet),
  enumerateScenariosWhere,
  scenarioMap,
  scenarios,
 )
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests)

class (Strategy p m, Show m) => HasParameterisedGenerator p m where
  parameterisedGenerator :: Set p -> Gen m

-- TODO caching calls to the solver in genSatisfying would probably be worth it
runGeneratorTest ::
  forall p m.
  (Eq p, Show p, HasParameterisedGenerator p m) =>
  Set p ->
  Property
runGeneratorTest s = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (m :: m) <- parameterisedGenerator s
      variablesSet m === s

runGeneratorTestsWhere ::
  (Show p, Ord p, HasParameterisedGenerator p m) =>
  String ->
  Formula p ->
  Group
runGeneratorTestsWhere name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, runGeneratorTest scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

genPropSet :: forall p a. (Ord p, Strategy p a) => Gen (Set p)
genPropSet = do
  let x = length (scenarios @p)
  i <- int (linear 0 (x - 1))
  case Map.lookup i scenarioMap of
    Nothing -> error "bad index in scenario sample this is a bug in apropos"
    Just set -> pure set

sampleGenTest ::
  forall p m.
  (Ord p, Show p, HasParameterisedGenerator p m) =>
  Property
sampleGenTest = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (ps :: Set p) <- genPropSet @p
      (m :: m) <- parameterisedGenerator ps
      variablesSet m === ps

enumerateGeneratorTest ::
  forall p m.
  (Ord p, Show p, HasParameterisedGenerator p m) =>
  Set p ->
  Property
enumerateGeneratorTest s =
  withTests (1 :: TestLimit) $
    property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      let (ms :: [m]) = enumerate $ parameterisedGenerator s
          run m = variablesSet m === s
      sequence_ (run <$> ms)

enumerateGeneratorTestsWhere ::
  (Ord p, Show p, HasParameterisedGenerator p m) =>
  String ->
  Formula p ->
  Group
enumerateGeneratorTestsWhere name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, enumerateGeneratorTest scenario)
    | scenario <- enumerateScenariosWhere condition
    ]

genSatisfying :: (Ord p, Show p, HasParameterisedGenerator p m) => Formula p -> Gen m
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  parameterisedGenerator s
