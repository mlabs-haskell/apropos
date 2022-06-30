module Apropos.Generator (
  runTest,
  decorateTests,
  selfTest,
  selfTestWhere,
) where

import Apropos.Description (Description (..), scenarios, variablesToDescription)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Hedgehog (Property, PropertyT, forAll, property, (===))

runTest :: (Show a, Description d a) => (a -> PropertyT IO ()) -> d -> Property
runTest cond d = property $ forAll (genDescribed d) >>= cond

decorateTests :: (IsString s, Show d) => Map d Property -> [(s, Property)]
decorateTests = map (first $ fromString . show) . Map.toList

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription :: forall d a. (Eq d, Show d, Show a, Description d a) => d -> Property
selfTestForDescription d = runTest (\a -> describe a === d) d

{- |
Test the lawfulness of a 'Description' instance.

The result type is @IsString s => [(s, Property)]@ so it can be plugged directly
into the Hedgehog 'Hedgehog.Group' constructor.
-}
selfTest :: forall d a s. (Ord d, Show d, Show a, Description d a, IsString s) => [(s, Property)]
selfTest = selfTestWhere @d (const True)

-- | Like 'selfTest', but you can filter which descriptions are tested.
selfTestWhere ::
  (Ord d, Show d, Show a, Description d a, IsString s) =>
  (d -> Bool) ->
  [(s, Property)]
selfTestWhere f =
  decorateTests
    . Map.fromSet selfTestForDescription
    . Set.filter f
    . Set.map variablesToDescription
    $ scenarios
