module Spec.IntPair (
  intPairGenTests,
  intPairGenPureTests,
  intPairGenSelfTests,
) where

import Apropos

import Control.Lens.Tuple (_1, _2)
import Control.Monad (join)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Spec.IntPermutationGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntPairProp
  = L IntProp
  | R IntProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntPairProp where
  logic = abstractionLogic @(Int, Int)

instance HasLogicalModel IntPairProp (Int, Int) where
  satisfiesProperty (L p) (i, _) = satisfiesProperty p i
  satisfiesProperty (R p) (_, i) = satisfiesProperty p i

instance HasAbstractions IntPairProp (Int, Int) where
  abstractions =
    [ WrapAbs $
        ProductAbstraction
          { abstractionName = "L"
          , propertyAbstraction = abstractsProperties L
          , productModelAbstraction = _1
          }
    , WrapAbs $
        ProductAbstraction
          { abstractionName = "R"
          , propertyAbstraction = abstractsProperties R
          , productModelAbstraction = _2
          }
    ]

instance HasPermutationGenerator IntPairProp (Int, Int) where
  generators = abstractionMorphisms

instance HasParameterisedGenerator IntPairProp (Int, Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen (Int, Int)
baseGen =
  (,) <$> genSatisfying (Yes :: Formula IntProp)
    <*> genSatisfying (Yes :: Formula IntProp)

intPairGenTests :: TestTree
intPairGenTests =
  testGroup "intPairGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              (Apropos :: (Int, Int) :+ IntPairProp)
              "(Int,Int) Generator"
              Yes
          ]

instance HasPureRunner IntPairProp (Int, Int) where
  expect _ =
    All $
      Var
        <$> join
          [ L <$> [IsSmall, IsNegative]
          , R <$> [IsSmall, IsPositive]
          ]
  script _ (l, r) = l < 0 && l >= -10 && r > 0 && r <= 10

intPairGenPureTests :: TestTree
intPairGenPureTests =
  testGroup "intPairGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere
              (Apropos :: (Int, Int) :+ IntPairProp)
              "AcceptsLeftSmallNegativeRightSmallPositive"
              Yes
          ]

intPairGenSelfTests :: TestTree
intPairGenSelfTests =
  testGroup "intPairGenSelfTests" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism IntPairProp (Int, Int)) -> True)
        baseGen
