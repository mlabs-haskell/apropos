module Apropos.Generator (
  runTest,
  decorateTests,
  selfTest,
  selfTestWhere,
) where

import Apropos.Description (
  Description (describe, genDescribed),
  scenarios,
  variablesToDescription,
 )
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy)
import Data.Set qualified as Set
import Data.String (IsString, fromString)
import Hedgehog (Property, PropertyT, forAll, property, (===))

runTest ::
  forall (a :: Type) (d :: Type).
  (Show a, Description d a) =>
  (a -> PropertyT IO ()) ->
  d ->
  Property
runTest cond d = property $ forAll (genDescribed d) >>= cond

decorateTests ::
  forall (s :: Type) (d :: Type).
  (IsString s, Show d) =>
  Map d Property ->
  [(s, Property)]
decorateTests = map (first $ fromString . show) . Map.toList

-- TODO caching calls to the solver in genSatisfying would probably be worth it
selfTestForDescription ::
  forall (d :: Type) (a :: Type).
  (Eq d, Show d, Show a, Description d a) =>
  d ->
  Property
selfTestForDescription d = runTest (\a -> describe a === d) d

{- |
Test the lawfulness of a 'Description' instance.

The result type is @IsString s => [(s, Property)]@ so it can be plugged directly
into the Hedgehog 'Hedgehog.Group' constructor.
-}
selfTest ::
  forall (d :: Type) (a :: Type) (s :: Type).
  (Ord d, Show d, Show a, Description d a, IsString s) =>
  Proxy d ->
  [(s, Property)]
selfTest _ = selfTestWhere @d (const True)

-- | Like 'selfTest', but you can filter which descriptions are tested.
selfTestWhere ::
  forall (d :: Type) (a :: Type) (s :: Type).
  (Ord d, Show d, Show a, Description d a, IsString s) =>
  (d -> Bool) ->
  [(s, Property)]
selfTestWhere f =
  decorateTests
    . Map.fromSet selfTestForDescription
    . Set.filter f
    . Set.map variablesToDescription
    $ scenarios
