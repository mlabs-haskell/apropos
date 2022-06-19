module Spec.IntCompact (
  intCompactGenTests,
  intCompactPureTests,
) where

import Apropos
import Apropos.Description
import Hedgehog (Group, assert)

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

intCompactGenTests :: Group
intCompactGenTests = selfTest @IntDescr

intCompactPureTests :: Group
intCompactPureTests =
  runPureTestsWhere @IntDescr
    (v [("Negative", 0)] "Small")
    (\i -> assert $ i < 0 && i >= -10)
    "AcceptsSmallNegativeInts"
    Yes
