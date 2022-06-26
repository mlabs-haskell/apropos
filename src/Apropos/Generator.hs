module Apropos.Generator (
  runTest,
  filteredTests,
  decorateTests,
  selfTest,
  selfTestWhere,
) where

import Apropos.Description (Description (..), scenarios, variablesToDescription)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Hedgehog (Property, PropertyT, forAll, property, (===))

runTest :: (Show a, Description d a) => (a -> PropertyT IO ()) -> d -> Property
runTest cond d = property $ forAll (genDescribed d) >>= cond

filteredTests :: forall d a. (Description d a, Ord d) => (d -> Bool) -> Set d
filteredTests f = Set.filter f . Set.map variablesToDescription $ scenarios @d

decorateTests :: (IsString s, Show d) => (d -> Property) -> Set d -> [(s, Property)]
decorateTests f = map (\d -> (fromString $ show d, f d)) . Set.toList

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription :: forall d a. (Eq d, Show d, Show a, Description d a) => d -> Property
selfTestForDescription d = runTest (\a -> describe a === d) d

selfTest :: forall d a s. (Ord d, Show d, Show a, Description d a, IsString s) => [(s, Property)]
selfTest = selfTestWhere @d (const True)

selfTestWhere ::
  forall d a s.
  (Ord d, Show d, Show a, Description d a, IsString s) =>
  (d -> Bool) ->
  [(s, Property)]
selfTestWhere = decorateTests selfTestForDescription . filteredTests
