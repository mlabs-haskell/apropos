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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntPairProp where
  logic = abstractionLogic @(Int, Int)

instance HasLogicalModel IntPairProp (Int, Int) where
  satisfiesProperty (L p) (i, _) = satisfiesProperty p i
  satisfiesProperty (R p) (_, i) = satisfiesProperty p i

instance HasAbstractions IntPairProp (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "make pair"
          , constructor = (,)
          , productAbs =
              ProductAbstraction
                { abstractionName = "L"
                , propertyAbstraction = abstractsProperties L
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = "R"
                  , propertyAbstraction = abstractsProperties R
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator IntPairProp (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator IntPairProp (Int, Int) where
  parameterisedGenerator = buildGen

intPairGenTests :: TestTree
intPairGenTests =
  testGroup "intPairGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              "(Int,Int) Generator"
              (Yes @IntPairProp)
          ]

instance HasPureRunner IntPairProp (Int, Int) where
  expect =
    All $
      Var
        <$> join
          [ L <$> [IsSmall, IsNegative]
          , R <$> [IsSmall, IsPositive]
          ]
  script (l, r) = l < 0 && l >= -10 && r > 0 && r <= 10

intPairGenPureTests :: TestTree
intPairGenPureTests =
  testGroup "intPairGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere
              "AcceptsLeftSmallNegativeRightSmallPositive"
              (Yes @IntPairProp)
          ]

intPairGenSelfTests :: TestTree
intPairGenSelfTests =
  testGroup "intPairGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @IntPairProp
