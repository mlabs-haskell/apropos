module Apropos.Generator (
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
  enumerateScenariosWhere,
  scenarioMap,
  scenarios,
 )
import Data.Map qualified as Map
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests)
import Apropos.Description (Description(..), VariableRep, DeepHasDatatypeInfo, variablesToDescription)

-- TODO caching calls to the solver in genSatisfying would probably be worth it
runGeneratorTest :: forall d a. (Eq d, Show d, Description d a) => d -> Property
runGeneratorTest s = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      m <- descriptionGen s
      describe m === s

runGeneratorTestsWhere ::
  forall d a.
  (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) =>
  String ->
  Formula (VariableRep d) ->
  Group
runGeneratorTestsWhere name condition =
  Group (fromString name) $
    [ (fromString $ show $ variablesToDescription scenario, runGeneratorTest (variablesToDescription scenario))
    | scenario <- enumerateScenariosWhere condition
    ]

genPropSet :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Gen d
genPropSet = do
  let x = length (scenarios @d)
  i <- int (linear 0 (x - 1))
  case Map.lookup i scenarioMap of
    Nothing -> error "bad index in scenario sample this is a bug in apropos"
    Just set -> pure (variablesToDescription set)

sampleGenTest :: forall d a. (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) => Property
sampleGenTest = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      ps <- genPropSet @d
      (m :: m) <- descriptionGen ps
      describe m === ps

enumerateGeneratorTest ::
  forall d a.
  (Eq d, Show d, Description d a) =>
  d ->
  Property
enumerateGeneratorTest s =
  withTests (1 :: TestLimit) $
    property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      let ms = enumerate $ descriptionGen @d s
          run m = describe m === s
      sequence_ (run <$> ms)

enumerateGeneratorTestsWhere ::
  forall d a.
  (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) =>
  String ->
  Formula (VariableRep d) ->
  Group
enumerateGeneratorTestsWhere name condition =
  Group (fromString name) $
    [ (fromString $ show $ variablesToDescription scenario, enumerateGeneratorTest @d (variablesToDescription scenario))
    | scenario <- enumerateScenariosWhere condition
    ]

genSatisfying :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Formula (VariableRep d) -> Gen a
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  descriptionGen (variablesToDescription s)
