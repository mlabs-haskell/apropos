module Spec.IntPairOverlay (
  smplPairTests,
) where

import Apropos

import Control.Lens.Tuple (_1, _2)
import Spec.IntOverlay
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup, testProperty)

data PairSmpl = BothNonNeg
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

data PairOfSmpl = L IntSmpl | R IntSmpl
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel PairSmpl where
  logic = Yes

instance HasAbstractions PairOfSmpl (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "pair"
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

instance LogicalModel PairOfSmpl where
  logic = abstractionLogic @(Int, Int)

instance HasLogicalModel PairOfSmpl (Int, Int) where
  satisfiesProperty (L p) (x, _) = satisfiesProperty p x
  satisfiesProperty (R p) (_, x) = satisfiesProperty p x

instance HasPermutationGenerator PairOfSmpl (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator PairOfSmpl (Int, Int) where
  parameterisedGenerator = buildGen

instance Overlay PairSmpl PairOfSmpl where
  overlays BothNonNeg = Var (L NonNegative) :&&: Var (R NonNegative)

instance HasLogicalModel PairSmpl (Int, Int) where
  satisfiesProperty = deduceFromOverlay

instance HasPermutationGenerator PairSmpl (Int, Int) where
  sources = overlaySources

instance HasParameterisedGenerator PairSmpl (Int, Int) where
  parameterisedGenerator = buildGen

smplPairTests :: TestTree
smplPairTests =
  testGroup
    "smplPairTests"
    [ testProperty "overlay is sound" $ soundOverlay @PairSmpl
    , fromGroup $ permutationGeneratorSelfTest @IntSmpl
    ]
