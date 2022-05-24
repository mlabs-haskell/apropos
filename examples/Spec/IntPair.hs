module Spec.IntPair (
  intPairGenTests,
  intPairGenPureTests,
  intPairGenSelfTests,
  intPairGenPureRunner,
) where

import Apropos
import Apropos.LogicalModel


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
  logic = unProp <$> abstractionLogic @(Int, Int) @(Prop IntPairProp)

instance HasLogicalModel IntPairProp (Int, Int) where
  satisfiesProperty (L p) (i, _) = satisfiesProperty p i
  satisfiesProperty (R p) (_, i) = satisfiesProperty p i

instance HasAbstractions (Prop IntPairProp) (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "make pair"
          , constructor = (,)
          , productAbs =
              ProductAbstraction 
                { abstractionName = "L"
                , propertyAbstraction = abstractsProperties (Prop . L . unProp)
                , productModelAbstraction = _1
                }
                :& ProductAbstraction
                  { abstractionName = "R"
                  , propertyAbstraction = abstractsProperties (Prop . R . unProp)
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance HasPermutationGenerator (Prop IntPairProp) (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator (Prop IntPairProp) (Int, Int) where
  parameterisedGenerator = buildGen

intPairGenTests :: TestTree
intPairGenTests =
  testGroup "intPairGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              "(Int,Int) Generator"
              (Yes @(Prop IntPairProp))
          ]

intPairGenPureRunner :: PureRunner (Prop IntPairProp) (Int, Int)
intPairGenPureRunner =
  PureRunner
    { expect =
        fmap Prop . All $
          Var
            <$> join
              [ L <$> [IsSmall, IsNegative]
              , R <$> [IsSmall, IsPositive]
              ]
    , script = \(l, r) -> l < 0 && l >= -10 && r > 0 && r <= 10
    }

intPairGenPureTests :: TestTree
intPairGenPureTests =
  testGroup "intPairGenPureTests" $
    fromGroup
      <$> [ runPureTestsWhere
              intPairGenPureRunner
              "AcceptsLeftSmallNegativeRightSmallPositive"
              (Yes @(Prop IntPairProp))
          ]

intPairGenSelfTests :: TestTree
intPairGenSelfTests =
  testGroup "intPairGenSelfTests" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop IntPairProp)
