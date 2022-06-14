module Spec.IntPermutationGen (
  intPermutationGenTests,
  intPermutationGenPureTests,
  intPermutationGenSelfTests,
  IntProp (..),
) where

import Apropos
import Apropos.LogicalModel
import Apropos.LogicalModel.HasLogicalModel (var)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  | IsMaxBound
  | IsMinBound
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)
      :&&: (Var IsMaxBound :->: (Var IsLarge :&&: Var IsPositive))
      :&&: (Var IsMinBound :->: (Var IsLarge :&&: Var IsNegative))

instance HasLogicalModel IntProp Int where
  satisfiesProperty IsNegative i = i < 0
  satisfiesProperty IsPositive i = i > 0
  satisfiesProperty IsMaxBound i = i == maxBound
  satisfiesProperty IsMinBound i = i == minBound
  satisfiesProperty IsZero i = i == 0
  satisfiesProperty IsLarge i = i > 10 || i < -10
  satisfiesProperty IsSmall i = i <= 10 && i >= -10

instance HasPermutationGenerator (Prop IntProp) Int where
  sources =
    [ Source
        { sourceName = "Zero"
        , covers = var IsZero
        , gen = pure 0
        }
    , Source
        { sourceName = "MaxBound"
        , covers = var IsMaxBound
        , gen = pure maxBound
        }
    , Source
        { sourceName = "MinBound"
        , covers = var IsMinBound
        , gen = pure minBound
        }
    , Source
        { sourceName = "Large"
        , covers = var IsLarge :&&: var IsPositive :&&: Not (var IsMaxBound)
        , gen = int (linear 11 (maxBound - 1))
        }
    , Source
        { sourceName = "Small"
        , covers = var IsSmall :&&: var IsPositive
        , gen = int (linear 1 10)
        }
    ]
  generators =
    [ Morphism
        { name = "Negate"
        , match = Not $ var IsZero
        , contract = swap (Prop IsNegative) (Prop IsPositive)
        , morphism = pure . negate
        }
    ]

instance HasParameterisedGenerator (Prop IntProp) Int where
  parameterisedGenerator = buildGen @(Prop IntProp)

intPermutationGenTests :: TestTree
intPermutationGenTests =
  testGroup "intPermutationGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere @(Prop IntProp) "Int Generator" Yes
          ]

intPermutationGenPureRunner :: PureRunner (Prop IntProp) Int
intPermutationGenPureRunner =
  PureRunner
    { expect = Var IsSmall :&&: Var IsNegative
    , script = \i -> i < 0 && i >= -10
    }

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere @(Prop IntProp) intPermutationGenPureRunner "AcceptsSmallNegativeInts" Yes
          ]

intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop IntProp)
