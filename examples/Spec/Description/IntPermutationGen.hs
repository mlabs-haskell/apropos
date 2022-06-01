module Spec.Description.IntPermutationGen (
  intPermutationGenTests,
  intPermutationGenPureTests,
  intPermutationGenSelfTests,
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

instance HasPermutationGenerator (VariableRep IntDescr) Int where
  sources =
    [ Source
        { sourceName = "v [(\"IntDescr\", \"sign\")] \"Zero\""
        , covers = v [("IntDescr", "sign")] "Zero"
        , gen = pure 0
        }
    , Source
        { sourceName = "v [(\"IntDescr\", \"sign\")] \"Positive\" :&&: v [(\"IntDescr\", \"isBound\")] \"True\""
        , covers = v [("IntDescr", "sign")] "Positive" :&&: v [("IntDescr", "isBound")] "True"
        , gen = pure maxBound
        }
    , Source
        { sourceName = "v [(\"IntDescr\", \"sign\")] \"Negative\" :&&: v [(\"IntDescr\", \"isBound\")] \"True\""
        , covers = v [("IntDescr", "sign")] "Negative" :&&: v [("IntDescr", "isBound")] "True"
        , gen = pure minBound
        }
    , Source
        { sourceName = "v [(\"IntDescr\", \"size\")] \"Large\""
        , covers = v [("IntDescr", "size")] "Large" :&&: v [("IntDescr", "sign")] "Positive" :&&: v [("IntDescr", "isBound")] "False"
        , gen = int (linear 11 (maxBound - 1))
        }
    , Source
        { sourceName = "v [(\"IntDescr\", \"size\")] \"Small\""
        , covers = v [("IntDescr", "size")] "Small" :&&: v [("IntDescr", "sign")] "Positive"
        , gen = int (linear 1 10)
        }
    ]
  generators =
    [ Morphism
        { name = "Negate"
        , match = Not $ v [("IntDescr", "sign")] "Zero"
        , contract = swap (V [("IntDescr", 0)] "Negative") (V [("IntDescr", 0)] "Positive")
        , morphism = pure . negate
        }
    ]

instance HasParameterisedGenerator (VariableRep IntDescr) Int where
  parameterisedGenerator = buildGen @(VariableRep IntDescr)

intPermutationGenTests :: TestTree
intPermutationGenTests =
  testGroup "intPermutationGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @(VariableRep IntDescr) "Int Generator" Yes
          ]

intPermutationGenPureRunner :: PureRunner (VariableRep IntDescr) Int
intPermutationGenPureRunner =
  PureRunner
    { expect = v [("IntDescr", "size")] "Small" :&&: v [("IntDescr", "sign")] "Negative"
    , script = \i -> i < 0 && i >= -10
    }

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intPermutationGenPureRunner "AcceptsSmallNegativeInts" Yes
          ]

intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(VariableRep IntDescr)