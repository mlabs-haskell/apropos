module Apropos.Runner (
  runTests,
  runTestsWhere,
  Outcome (Pass, Fail),
  passIf,
  OptOutcome (Run, Ignore),
) where

import Apropos.Description (Description, scenarios, variablesToDescription)
import Apropos.Generator (decorateTests, runTest)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Bool (bool)
import Data.Either (isRight)
import Data.Kind (Type)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (IsString)
import Hedgehog (Property, PropertyT, (===))
import Hedgehog.Internal.Property (PropertyT (PropertyT), TestT (TestT, unTest), unPropertyT)

-- | Whether a test should pass or fail
data Outcome = Pass | Fail
  deriving stock (Eq, Show)

-- | Construct an 'Outcome' from a 'Bool' predicate
passIf :: Bool -> Outcome
passIf = bool Pass Fail

-- | Whether a test should be ignored
data OptOutcome
  = Run Outcome
  | Ignore

optOutcomeToMaybe :: OptOutcome -> Maybe Outcome
optOutcomeToMaybe Ignore = Nothing
optOutcomeToMaybe (Run o) = Just o

runAproposTest :: forall (d :: Type) (a :: Type). (Description d a, Show a) => Outcome -> (a -> PropertyT IO ()) -> d -> Property
runAproposTest expect test =
  runTest
    ( \a -> do
        b <- passes (test a)
        expect === b
    )
  where
    passes :: PropertyT IO () -> PropertyT IO Outcome
    passes =
      PropertyT
        . TestT
        . ExceptT
        . fmap (Right . passIf . isRight)
        . runExceptT
        . unTest
        . unPropertyT

{- |

Run Apropos tests.

Apropos is able to test both positive and negative cases. By returning 'Fail'
from the predicate, you can expect the test to fail for a given description.

To ignore descriptions entirely, use 'runTestsWhere'.
-}
runTests ::
  forall (d :: Type) (a :: Type) (s :: Type).
  (Show d, Show a, Ord d, Description d a, IsString s) =>
  -- | Should the test pass or fail?
  (d -> Outcome) ->
  -- | The test to run
  (a -> PropertyT IO ()) ->
  [(s, Property)]
runTests f = runTestsWhere (Run . f)

{- |

Run Apropos tests on a subset of descriptions.
-}
runTestsWhere ::
  forall (d :: Type) (a :: Type) (s :: Type).
  (Show d, Show a, Ord d, Description d a, IsString s) =>
  -- | Should the test pass, fail, or be ignored?
  (d -> OptOutcome) ->
  -- | The test to run
  (a -> PropertyT IO ()) ->
  [(s, Property)]
runTestsWhere expect test =
  decorateTests
    . Map.mapMaybeWithKey (\d () -> (\b -> runAproposTest b test d) <$> optOutcomeToMaybe (expect d))
    . Map.fromSet (const ())
    . Set.map variablesToDescription
    $ scenarios @d
