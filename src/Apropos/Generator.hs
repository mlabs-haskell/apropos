module Apropos.Generator (
  selfTest,
  selfTestWhere,
  exhaustiveSelfTest,
  exhaustiveSelfTestWhere,
  genSatisfying,
  sampleGenTest,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Gen
import Apropos.Gen.Enumerate (Exhaustivity (..), exhaustivityLabel, runTest)
import Apropos.Logic (
  Formula (..),
  enumerateScenariosWhere,
  scenarioMap,
  scenarios,
 )
import Data.Map qualified as Map
import Data.String (fromString)
import Generics.SOP (Proxy (Proxy), datatypeInfo, datatypeName)
import Hedgehog (Group (..), Property, property)

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription :: forall d a. (Eq d, Show d, Description d a) => Exhaustivity -> d -> Property
selfTestForDescription ex s = runTest ex (descriptionGen s) (\m -> describe m === s)

selfTest' :: forall d a. (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) => Exhaustivity -> Group
selfTest' ex = selfTestWhere' @d ex Yes

selfTestWhere' ::
  forall d a.
  (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) =>
  Exhaustivity ->
  Formula (VariableRep d) ->
  Group
selfTestWhere' ex condition =
  Group (fromString $ datatypeName (datatypeInfo @d Proxy) ++ " " ++ exhaustivityLabel ex ++ " self test") $
    [ (fromString $ show $ variablesToDescription scenario, selfTestForDescription ex (variablesToDescription scenario))
    | scenario <- enumerateScenariosWhere condition
    ]

selfTest :: forall d a. (Description d a, DeepHasDatatypeInfo d, Ord d, Show d) => Group
selfTest = selfTest' @d Probablistic

selfTestWhere :: (Description d a, DeepHasDatatypeInfo d, Ord d, Show d) => Formula (VariableRep d) -> Group
selfTestWhere = selfTestWhere' Probablistic

exhaustiveSelfTest :: forall d a. (Description d a, DeepHasDatatypeInfo d, Ord d, Show d) => Group
exhaustiveSelfTest = selfTest' @d Exhaustive

exhaustiveSelfTestWhere :: (Description d a, DeepHasDatatypeInfo d, Ord d, Show d) => Formula (VariableRep d) -> Group
exhaustiveSelfTestWhere = selfTestWhere' Exhaustive

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

genSatisfying :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Formula (VariableRep d) -> Gen a
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  descriptionGen (variablesToDescription s)
