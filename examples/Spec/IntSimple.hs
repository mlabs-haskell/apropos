{-# LANGUAGE OverloadedLists #-}

module Spec.IntSimple (
  intSimpleSelfTest,
  intSimpleBadProperty,
  intSimpleExampleUnit,
  intSimpleAproposExample,
) where

import Apropos(
  Description(describe, refineDescription, genDescribed),
  Formula(All, (:->:)),
  attr,
  selfTest,
  runTests,
  )

import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
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
  , isBound :: Bool -- Is this equsl to 'minBound' or 'maxBound'?
  }
  deriving stock (Generic, Eq, Ord, Show) -- These are required, unfortunately.

data Sign = Positive | Negative | Zero
  deriving stock (Generic, Eq, Ord, Show)

data Size = Large | Small
  deriving stock (Generic, Eq, Ord, Show)

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
  refineDescription :: Formula IntDescr
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
      s' -> intGen s'
    where
      bound :: Sign -> Int
      bound Positive = maxBound
      bound Negative = minBound
      bound Zero = 0

      sig :: Sign -> Int -> Int
      sig Negative = negate
      sig _ = id

      intGen :: (MonadGen m) => Sign -> m Int
      intGen s' =
        if isBound s
          then pure (bound s')
          else case size s of
            Small -> int (linear (sig s' 1) (sig s' 10))
            Large -> int (linear (sig s' 11) (bound s' - sig s' 1))

-- Let's first test that our instance is lawful.
intSimpleSelfTest :: Group
intSimpleSelfTest =
  Group
    "self test"
    (selfTest $ Proxy @IntDescr)

-- And we catch our bug!
intSimpleAproposExample :: Group
intSimpleAproposExample =
  Group
    "apropos testing"
    $ runTests @IntDescr
      (const True) -- should hold for all negative integers
      (assert . absIsAlwaysPositive)
