module Apropos.Generator (
  runTest,
  selfTest,
  selfTestWhere,
  genSatisfying,
  sampleGenTest,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), VariableRep, enumerateScenariosWhere, scenarios, variablesToDescription)
import Apropos.Formula (Formula (..))
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Hedgehog (Gen, Property, PropertyT, forAll, label, property, (===))
import Hedgehog.Gen (element)

runTest :: (Show a, Description d a) => (a -> PropertyT IO ()) -> d -> Property
runTest cond d = property $ forAll (genForDescription d) >>= cond

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription :: forall d a. (Eq d, Show d, Show a, Description d a) => d -> Property
selfTestForDescription d = runTest (\a -> describe a === d) d

selfTest :: forall d a s. (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d, IsString s) => [(s, Property)]
selfTest = selfTestWhere @d Yes

selfTestWhere ::
  forall d a s.
  (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d, IsString s) =>
  Formula (VariableRep d) ->
  [(s, Property)]
selfTestWhere condition =
  [ (fromString $ show $ variablesToDescription scenario, selfTestForDescription (variablesToDescription scenario))
  | scenario <- Set.toList $ enumerateScenariosWhere condition
  ]

descriptionGen :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Gen d
descriptionGen = variablesToDescription <$> element (Set.toList scenarios)

sampleGenTest :: forall d a. (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d) => Property
sampleGenTest =
  property $ do
    ps <- forAll $ descriptionGen @d
    (m :: m) <- forAll $ genForDescription ps
    describe m === ps

genSatisfying :: forall d a m. (Description d a, DeepHasDatatypeInfo d, Monad m, Show a) => Formula (VariableRep d) -> PropertyT m a
genSatisfying f = do
  label $ fromString $ show f
  s <- forAll $ element (Set.toList $ enumerateScenariosWhere f)
  forAll $ genForDescription (variablesToDescription s)
