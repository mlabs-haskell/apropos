module Spec.IntCompact (
  intCompactGenTests,
  intCompactPureTests,
) where

import Apropos
import Apropos.Description
import Hedgehog (Group, assert)
import Hedgehog.Gen (int)
import Hedgehog.Range (linear)

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

  genForDescription Zero = pure 0
  genForDescription (Positive (Large True)) = pure maxBound
  genForDescription (Positive (Large False)) = int (linear 11 (maxBound - 1))
  genForDescription (Positive Small) = int (linear 1 10)
  genForDescription (Negative (Large True)) = pure minBound
  genForDescription (Negative (Large False)) = int (linear (minBound + 1) (-11))
  genForDescription (Negative Small) = int (linear (-10) (-1))

intCompactGenTests :: Group
intCompactGenTests = selfTest @IntDescr

intCompactPureTests :: Group
intCompactPureTests =
  runTests @IntDescr
    "AcceptsSmallNegativeInts" $
    AproposTest
      { expect = \case
          Negative Small -> True
          _ -> False
      , test   = \i -> assert $ i < 0 && i >= -10
      }