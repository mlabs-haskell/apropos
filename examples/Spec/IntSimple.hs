module Spec.IntSimple (
  intSimpleSelfTest,
  intSimpleBadProperty,
  intSimpleExampleUnit,
  intSimpleAproposExample,
) where

import Apropos
import Hedgehog (Group (Group), MonadGen, Property, assert, forAll, property)
import Hedgehog.Gen (int)
import Hedgehog.Range (linear)
import Test.Tasty.HUnit (Assertion, assertBool)

-- This example is based on https://github.com/nick8325/quickcheck/issues/98, and is due to our very own Baldur Blöndal.

-- This should always return true. But it has a bug!
absIsAlwaysPositive :: Int -> Bool
absIsAlwaysPositive n = abs n >= 0

-- abs minBound == minBound :-(
intSimpleExampleUnit :: Assertion
intSimpleExampleUnit = assertBool "abs minBound >= 0" (absIsAlwaysPositive minBound)

-- A naive property test is unlikely to catch this.
intSimpleBadProperty :: Property
intSimpleBadProperty =
  property $ forAll (int (linear 0 minBound)) >>= assert . absIsAlwaysPositive

-- Let's define a type that captures the interesting properties of 'Int's.
data IntDescr = IntDescr
  { sign :: Sign
  , size :: Size
  , isBound :: Bool -- Is this 'minBound' or 'maxBound'?
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo) -- These are required, unfortunately.

data Sign = Positive | Negative | Zero
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Size = Large | Small
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

instance Description IntDescr Int where
  -- Describe an 'Int'
  describe :: Int -> IntDescr
  describe i =
    IntDescr
      { sign =
          case compare i 0 of
            GT -> Positive
            EQ -> Zero
            LT -> Negative
      , size =
          if i > 10 || i < -10
            then Large
            else Small
      , isBound = i == minBound || i == maxBound
      }

  -- Not all 'IntDescr's are valid. Let's define which ones are.
  refineDescription :: Formula (Attribute IntDescr)
  refineDescription =
    All
      [ attr [("IntDescr", "sign")] "Zero" :->: attr [("IntDescr", "size")] "Small"
      , attr [("IntDescr", "isBound")] "True" :->: attr [("IntDescr", "size")] "Large"
      ]

  -- We define how to generate values matching a given description.
  genDescribed :: (MonadGen m) => IntDescr -> m Int
  genDescribed s =
    case sign s of
      Zero -> pure 0
      Positive -> intGen
      Negative -> intGen
    where
      bound :: Int
      sig :: Int -> Int
      (bound, sig) =
        case sign s of
          Positive -> (maxBound, id)
          Negative -> (minBound, negate)
          Zero -> (0, id)

      intGen :: (MonadGen m) => m Int
      intGen =
        if isBound s
          then pure bound
          else case size s of
            Small -> int (linear (sig 1) (sig 10))
            Large -> int (linear (sig 11) (bound + sig (-1)))

-- Let's first test that our instance is lawful.
intSimpleSelfTest :: Group
intSimpleSelfTest =
  Group
    "self test"
    (selfTest @IntDescr)

-- And we catch our bug!
intSimpleAproposExample :: Group
intSimpleAproposExample =
  Group
    "apropos testing"
    $ runTests @IntDescr
      AproposTest
        { expect = const True -- should hold for all negative integers
        , aproposTest = assert . absIsAlwaysPositive
        }
