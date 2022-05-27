module Spec.IntPermutationGen (
  intPermutationGenTests,
  intPermutationGenPureTests,
  intPermutationGenSelfTests,
  IntProp (..),
) where

import Apropos
import Apropos.LogicalModel
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
        , covers = Var (Prop IsZero)
        , gen = pure 0
        }
    , Source
        { sourceName = "MaxBound"
        , covers = Var (Prop IsMaxBound)
        , gen = pure maxBound
        }
    , Source
        { sourceName = "MinBbound"
        , covers = Var (Prop IsMinBound)
        , gen = pure minBound
        }
    , Source
        { sourceName = "Large"
        , covers = Var (Prop IsLarge) :&&: Var (Prop IsPositive) :&&: Not (Var (Prop IsMaxBound))
        , gen = int (linear 11 (maxBound - 1))
        }
    , Source
        { sourceName = "Small"
        , covers = Var (Prop IsSmall) :&&: Var (Prop IsPositive)
        , gen = int (linear 1 10)
        }
    ]
  generators =
    [ Morphism
        { name = "Negate"
        , match = Not $ Var (Prop IsZero)
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
      <$> [ runGeneratorTestsWhere "Int Generator" (Yes @(Prop IntProp))
          ]

intPermutationGenPureRunner :: PureRunner (Prop IntProp) Int
intPermutationGenPureRunner =
  PureRunner
    { expect = Var (Prop IsSmall) :&&: Var (Prop IsNegative)
    , script = \i -> i < 0 && i >= -10
    }

intPermutationGenPureTests :: TestTree
intPermutationGenPureTests =
  testGroup "intPermutationGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intPermutationGenPureRunner "AcceptsSmallNegativeInts" (Yes @(Prop IntProp))
          ]

intPermutationGenSelfTests :: TestTree
intPermutationGenSelfTests =
  testGroup "intPermutationGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop IntProp)
