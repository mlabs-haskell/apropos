module Apropos.Runner (
  runTests,
  runTestsWhere,
) where

import Apropos.Description (Description, scenarios, variablesToDescription)
import Apropos.Generator (decorateTests, runTest)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Either (isRight)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (IsString)
import Hedgehog (Property, PropertyT, (===))
import Hedgehog.Internal.Property (PropertyT (PropertyT), TestT (TestT, unTest), unPropertyT)

runAproposTest :: forall (d :: Type) (a :: Type). (Description d a, Show a) => Bool -> (a -> PropertyT IO ()) -> d -> Property
runAproposTest expect test =
  runTest
    ( \a -> do
        b <- passes (test a)
        expect === b
    )
  where
    passes :: PropertyT IO () -> PropertyT IO Bool
    passes =
      PropertyT
        . TestT
        . ExceptT
        . fmap (Right . isRight)
        . runExceptT
        . unTest
        . unPropertyT

{- |

Run Apropos tests.

Apropos is able to test both positive and negative cases. By returning 'False'
from the predicate, you can expect the test to fail for a given description.

To ignore descriptions entirely, use 'runTestsWhere'.
-}
runTests ::
  forall (d :: Type) (a :: Type) (s :: Type).
  (Show d, Show a, Ord d, Description d a, IsString s) =>
  -- | Should the test pass or fail?
  (d -> Bool) ->
  -- | The test to run
  (a -> PropertyT IO ()) ->
  [(s, Property)]
runTests f = runTestsWhere (Just . f)

{- |

Run Apropos tests on a subset of descriptions.

This function is provided with a predicate of type @d -> Maybe Bool@. 'Nothing'
causes the description to be ignored, 'Just True' tests that it causes the test
to pass, and 'Just False' tests that it causes it to fail.
-}
runTestsWhere ::
  forall (d :: Type) (a :: Type) (s :: Type).
  (Show d, Show a, Ord d, Description d a, IsString s) =>
  -- | Should the test pass, fail, or be ignored?
  (d -> Maybe Bool) ->
  -- | The test to run
  (a -> PropertyT IO ()) ->
  [(s, Property)]
runTestsWhere expect test =
  decorateTests
    . Map.mapMaybeWithKey (\d () -> (\b -> runAproposTest b test d) <$> expect d)
    . Map.fromSet (const ())
    . Set.map variablesToDescription
    $ scenarios @d
