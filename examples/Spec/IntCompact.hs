module Spec.IntCompact (
  intCompactSelfTest,
  intCompactExampleUnit,
  intCompactAproposExample,
) where

import Apropos (
  Description (describe, genDescribed),
  Outcome(Pass, Fail),
  runTests,
  selfTest,
 )
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Hedgehog (Group (Group), assert)
import Hedgehog.Gen (int)
import Hedgehog.Range (linear)
import Test.Tasty.HUnit (Assertion, assertBool)

-- This is a variant of 'IntSimple', demonstrating a different way of building description types. Also, for variety, we switched the test up to show conditional testing.

-- We've worked a bit harder defining the description, and can capture all the logic in the type. It's now impossible to construct a Large Zero or Small isBound.
type IntDescr :: Type
data IntDescr
  = Zero
  | Positive Size
  | Negative Size
  deriving stock (Show, Eq, Ord, Generic)

type Size :: Type
data Size = Small | Large {isBound :: Bool}
  deriving stock (Show, Eq, Ord, Generic)

instance Description IntDescr Int where
  -- 'describe' is arguably simpler.
  describe :: Int -> IntDescr
  describe 0 = Zero
  describe i
    | i > 0 = Positive size
    | otherwise = Negative size
    where
      size :: Size
      size
        | i < 11 && i > -111 = Small
        | otherwise = Large {isBound = i == minBound || i == maxBound}

  -- no need for 'refineDescription' here!

  -- Also maybe a bit more straightforward.
  genDescribed = \case
    Zero -> pure 0
    Positive (Large True) -> pure maxBound
    Positive (Large False) -> int (linear 11 (maxBound - 1))
    Positive Small -> int (linear 1 10)
    Negative (Large True) -> pure minBound
    Negative (Large False) -> int (linear (minBound + 1) (-11))
    Negative Small -> int (linear (-10) (-1))

-- This should hold for all positive integers, but no negative integers or zero.
hasNegativeNegation :: Int -> Bool
hasNegativeNegation n = negate n < 0

-- it doesn't, unfortunately.
intCompactExampleUnit :: Assertion
intCompactExampleUnit = assertBool "negate minBound >= 0" (not $ hasNegativeNegation minBound)

intCompactSelfTest :: Group
intCompactSelfTest =
  Group
    "self test"
    (selfTest $ Proxy @IntDescr)

-- Not only does 'apropos' test for values that have the given properties, it also
-- ensures that those without them fail the test.
intCompactAproposExample :: Group
intCompactAproposExample =
  Group
    "apropos testing"
    $ runTests @IntDescr
      ( \case
          Positive _ -> Pass -- all positive values should pass.
          _ -> Fail -- all other values should fail!
      )
      (assert . hasNegativeNegation)
