{-# LANGUAGE TemplateHaskell #-}

module Spec.IntPair (
  intPairGenTests,
  intPairGenPureTests,
  intPairGenSelfTests,
) where

import Apropos

import Control.Lens.Tuple (_1, _2)
import Control.Monad (join)
import Spec.IntPermutationGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntPairProp
  = L IntProp
  | R IntProp
  deriving stock (Eq, Ord, Show)

$(genEnumerable ''IntPairProp)

instance LogicalModel IntPairProp where
  logic = (L <$> logic) :&&: (R <$> logic)

instance HasLogicalModel IntPairProp (Int, Int) where
  satisfiesProperty (L p) (i, _) = satisfiesProperty p i
  satisfiesProperty (R p) (_, i) = satisfiesProperty p i

instance HasPermutationGenerator IntPairProp (Int, Int) where
  generators =
    let l =
          Abstraction
            { abstractionName = "L"
            , propertyAbstraction = abstractsProperties L
            , modelAbstraction = _1
            }
        r =
          Abstraction
            { abstractionName = "R"
            , propertyAbstraction = abstractsProperties R
            , modelAbstraction = _2
            }
     in join [abstract l <$> generators, abstract r <$> generators]

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
