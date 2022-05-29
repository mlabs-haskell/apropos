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
  Strategy (..),
  enumerateScenariosWhere,
  scenarioMap,
  scenarios,
 )
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (Group (..), Property, TestLimit, property, withTests)

class (Strategy p m, Show m) => HasParameterisedGenerator p m where
  parameterisedGenerator :: Properties p -> Gen m

-- TODO caching calls to the solver in genSatisfying would probably be worth it
runGeneratorTest ::
  forall p m.
  (Eq (Properties p), Show (Properties p), HasParameterisedGenerator p m) =>
  Properties p ->
  Property
runGeneratorTest s = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (m :: m) <- parameterisedGenerator @p s
      toProperties @p m === s

runGeneratorTestsWhere ::
  forall p m.
  (Ord p, Eq (Properties p), Show (Properties p), HasParameterisedGenerator p m) =>
  String ->
  Formula p ->
  Group
runGeneratorTestsWhere name condition =
  Group (fromString name) $
    [ (fromString $ show $ variablesToProperties scenario, runGeneratorTest @p (variablesToProperties scenario))
    | scenario <- enumerateScenariosWhere condition
    ]

genPropSet :: forall p a. (Ord p, Strategy p a) => Gen (Properties p)
genPropSet = do
  let x = length (scenarios @p)
  i <- int (linear 0 (x - 1))
  case Map.lookup i (scenarioMap @p) of
    Nothing -> error "bad index in scenario sample this is a bug in apropos"
    Just set -> pure (variablesToProperties set)

sampleGenTest ::
  forall p m.
  (Ord p, Ord (Properties p), Show (Properties p), HasParameterisedGenerator p m) =>
  Property
sampleGenTest = property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      (ps :: Properties p) <- genPropSet @p
      (m :: m) <- parameterisedGenerator @p ps
      toProperties @p m === ps

enumerateGeneratorTest ::
  forall p m.
  (Eq (Properties p), Show (Properties p), HasParameterisedGenerator p m) =>
  Properties p ->
  Property
enumerateGeneratorTest s =
  withTests (1 :: TestLimit) $
    property $ runGenModifiable test >>= errorHandler
  where
    test = forAll $ do
      let (ms :: [m]) = enumerate $ parameterisedGenerator @p s
          run m = toProperties @p m === s
      sequence_ (run <$> ms)

enumerateGeneratorTestsWhere ::
  forall p m.
  (Ord p, Show p, Eq (Properties p), Show (Properties p), HasParameterisedGenerator p m) =>
  String ->
  Formula p ->
  Group
enumerateGeneratorTestsWhere name condition =
  Group (fromString name) $
    [ (fromString $ show $ Set.toList scenario, enumerateGeneratorTest @p (variablesToProperties scenario))
    | scenario <- enumerateScenariosWhere condition
    ]

genSatisfying :: forall p m. (Ord p, Show p, HasParameterisedGenerator p m) => Formula p -> Gen m
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  parameterisedGenerator @p (variablesToProperties s)
