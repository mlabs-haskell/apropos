module Apropos.Generator (
  selfTest,
  selfTestWhere,
  genSatisfying,
  sampleGenTest,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, variablesToDescription)
import Apropos.Logic (
  Formula (..),
  enumerateScenariosWhere,
  runTest,
  scenarioMap,
  scenarios,
 )
import Data.Map qualified as Map
import Data.String (fromString)
import Generics.SOP (Proxy (Proxy), datatypeInfo, datatypeName)
import Hedgehog (Gen, Group (..), Property, PropertyT, forAll, label, property, (===))
import Hedgehog.Gen (element, int)
import Hedgehog.Range (linear)

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription :: forall d a. (Eq d, Show d, Show a, Description d a) => d -> Property
selfTestForDescription s = runTest (genForDescription s) (\m -> describe m === s)

selfTest :: forall d a. (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d) => Group
selfTest = selfTestWhere @d Yes

selfTestWhere ::
  forall d a.
  (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d) =>
  Formula (VariableRep d) ->
  Group
selfTestWhere condition =
  Group (fromString (datatypeName (datatypeInfo @d Proxy)) <> " self test") $
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

sampleGenTest :: forall d a. (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d) => Property
sampleGenTest =
  property $ do
    ps <- forAll $ genPropSet @d
    (m :: m) <- forAll $ genForDescription ps
    describe m === ps

genSatisfying :: forall d a m. (Description d a, DeepHasDatatypeInfo d, Monad m, Show a) => Formula (VariableRep d) -> PropertyT m a
genSatisfying f = do
  label $ fromString $ show f
  s <- forAll $ element (enumerateScenariosWhere f)
  forAll $ genForDescription (variablesToDescription s)
