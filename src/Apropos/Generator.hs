module Apropos.Generator (
  runTest,
  selfTest,
  selfTestWhere,
) where

import Apropos.Description (DeepHasDatatypeInfo, Description (..), Attribute, enumerateScenariosWhere, variablesToDescription)
import Apropos.Formula (Formula (..))
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Hedgehog (Property, PropertyT, forAll, property, (===))

runTest :: (Show a, Description d a) => (a -> PropertyT IO ()) -> d -> Property
runTest cond d = property $ forAll (genDescribed d) >>= cond

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription :: forall d a. (Eq d, Show d, Show a, Description d a) => d -> Property
selfTestForDescription d = runTest (\a -> describe a === d) d

selfTest :: forall d a s. (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d, IsString s) => [(s, Property)]
selfTest = selfTestWhere @d Yes

selfTestWhere ::
  forall d a s.
  (Ord d, Show d, Show a, Description d a, DeepHasDatatypeInfo d, IsString s) =>
  Formula (Attribute d) ->
  [(s, Property)]
selfTestWhere condition =
  [ (fromString $ show $ variablesToDescription scenario, selfTestForDescription (variablesToDescription scenario))
  | scenario <- Set.toList $ enumerateScenariosWhere condition
  ]
