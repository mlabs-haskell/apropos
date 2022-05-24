module Spec.Int (HasLogicalModel (..), IntProp (..), intGenTests, intPureTests, intPureRunner) where

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
  deriving anyclass (Enumerable)

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

instance HasParameterisedGenerator (Prop IntProp) Int where
  parameterisedGenerator s = do
    i <-
      if Prop IsZero `elem` s
        then pure 0
        else
          if Prop IsSmall `elem` s
            then int (linear 1 10)
            else
              if Prop IsMaxBound `elem` s
                then pure maxBound
                else int (linear 11 (maxBound - 1))
    if Prop IsNegative `elem` s
      then
        if Prop IsMinBound `elem` s
          then pure minBound
          else pure (-i)
      else pure i

intGenTests :: TestTree
intGenTests =
  testGroup "intGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere "Int Generator" (Yes @(Prop IntProp))
          ]

intPureRunner :: PureRunner (Prop IntProp) Int
intPureRunner =
  PureRunner
    { expect = Var (Prop IsSmall) :&&: Var (Prop IsNegative)
    , script = \i -> i < 0 && i >= -10
    }

intPureTests :: TestTree
intPureTests =
  testGroup "intPureTests" $
    fromGroup
      <$> [ runPureTestsWhere intPureRunner "AcceptsSmallNegativeInts" (Yes @(Prop IntProp))
          ]
