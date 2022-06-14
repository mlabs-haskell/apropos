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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Size = Small | Large {isBound :: Bool}
  deriving stock (Show, Eq, Generic)
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

instance HasParameterisedGenerator (VariableRep IntDescr) Int where
  parameterisedGenerator Zero = pure 0
  parameterisedGenerator (Positive (Large True)) = pure maxBound
  parameterisedGenerator (Positive (Large False)) = int (linear 11 (maxBound - 1))
  parameterisedGenerator (Positive Small) = int (linear 1 10)
  parameterisedGenerator (Negative (Large True)) = pure minBound
  parameterisedGenerator (Negative (Large False)) = int (linear (minBound + 1) (-11))
  parameterisedGenerator (Negative Small) = int (linear (-10) (-1))

intCompactGenTests :: TestTree
intCompactGenTests =
  testGroup "intGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @(VariableRep IntDescr) "Int Generator" Yes
          ]

intCompactPureRunner :: PureRunner (VariableRep IntDescr) Int
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
