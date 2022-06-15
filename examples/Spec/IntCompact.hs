module Spec.IntCompact (
  intCompactGenTests,
  intCompactPureTests,
) where

import Apropos
import Apropos.Description
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntDescr
  = Zero
  | Positive Size
  | Negative Size
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Size = Small | Large {isBound :: Bool}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

instance Description IntDescr Int where
  describe 0 = Zero
  describe i
    | i > 0 = Positive size
    | otherwise = Negative size
    where
      size :: Size
      size
        | i < 11 && i > -111 = Small
        | otherwise = Large {isBound = i == minBound || i == maxBound}

  descriptionGen Zero = pure 0
  descriptionGen (Positive (Large True)) = pure maxBound
  descriptionGen (Positive (Large False)) = int (linear 11 (maxBound - 1))
  descriptionGen (Positive Small) = int (linear 1 10)
  descriptionGen (Negative (Large True)) = pure minBound
  descriptionGen (Negative (Large False)) = int (linear (minBound + 1) (-11))
  descriptionGen (Negative Small) = int (linear (-10) (-1))

intCompactGenTests :: TestTree
intCompactGenTests =
  testGroup "intGenTests" $
    fromGroup
      <$> [ selfTest @IntDescr
          ]

intCompactPureRunner :: PureRunner IntDescr Int
intCompactPureRunner =
  PureRunner
    { expect = v [("Negative", 0)] "Small"
    , script = \i -> i < 0 && i >= -10
    }

intCompactPureTests :: TestTree
intCompactPureTests =
  testGroup "intCompactPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intCompactPureRunner "AcceptsSmallNegativeInts" Yes
          ]
