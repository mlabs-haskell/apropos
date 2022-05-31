module Spec.Description.IntSimple (
  intSimpleGenTests,
  intSimplePureTests,
) where

import Apropos
import Apropos.Description
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntDescr = IntDescr
  { sign :: Sign
  , size :: Size
  , isBound :: Bool
  }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Sign = Positive | Negative | Zero
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

data Size = Large | Small
  deriving stock (Generic, Eq, Show)
  deriving anyclass (SOPGeneric, HasDatatypeInfo)

instance Description IntDescr Int where
  describe i =
    IntDescr
      { sign =
          if i < 0
            then Negative
            else
              if i == 0
                then Zero
                else Positive
      , size =
          if i > 10 || i < -10
            then Large
            else Small
      , isBound = i == minBound || i == maxBound
      }

  additionalLogic =
    All
      [ v [("IntDescr", "sign")] "Zero" :->: v [("IntDescr", "size")] "Small"
      , v [("IntDescr", "isBound")] "True" :->: v [("IntDescr", "size")] "Large"
      ]

instance HasParameterisedGenerator (VariableRep IntDescr) Int where
  parameterisedGenerator s =
    case sign s of
      Zero -> pure 0
      Positive ->
        if isBound s
          then pure maxBound
          else intGen (size s)
      Negative ->
        if isBound s
          then pure minBound
          else negate <$> intGen (size s)
    where
      intGen :: Size -> Gen Int
      intGen Small = int (linear 1 10)
      intGen Large = int (linear 11 (maxBound - 1))

intSimpleGenTests :: TestTree
intSimpleGenTests =
  testGroup "intGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @(VariableRep IntDescr) "Int Generator" Yes
          ]

intSimplePureRunner :: PureRunner (VariableRep IntDescr) Int
intSimplePureRunner =
  PureRunner
    { expect = v [("IntDescr", "size")] "Small" :&&: v [("IntDescr", "sign")] "Negative"
    , script = \i -> i < 0 && i >= -10
    }

intSimplePureTests :: TestTree 
intSimplePureTests =
  testGroup "intSimplePureTests" $
    fromGroup
      <$> [ runPureTestsWhere intSimplePureRunner "AcceptsSmallNegativeInts" Yes
          ]