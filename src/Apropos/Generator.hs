module Apropos.Generator (
  selfTest,
  selfTestWhere,
  genSatisfying,
  sampleGenTest,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Gen
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
selfTestForDescription :: forall d a. (Eq d, Show d, Description d a) => d -> Property
selfTestForDescription s = runTest (descriptionGen s) (\m -> describe m === s)

selfTest :: forall d a. (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) => Group
selfTest = selfTestWhere @d Yes

selfTestWhere ::
  forall d a.
  (Ord d, Show d, Description d a, DeepHasDatatypeInfo d) =>
  Formula (VariableRep d) ->
  Group
selfTestWhere condition =
  Group (fromString $ datatypeName (datatypeInfo @d Proxy) ++ " self test") $
    [ (fromString $ show $ variablesToDescription scenario, selfTestForDescription (variablesToDescription scenario))
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
sampleGenTest =
  property $ forAllApropos $ do
    ps <- genPropSet @d
    (m :: m) <- descriptionGen ps
    describe m === ps

genSatisfying :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Formula (VariableRep d) -> Gen a
genSatisfying f = do
  label $ fromString $ show f
  s <- element (enumerateScenariosWhere f)
  descriptionGen (variablesToDescription s)
