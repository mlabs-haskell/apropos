module Spec.IntPairOverlay (
  smplPairTests,
) where

import Apropos
import Apropos.LogicalModel

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

instance HasAbstractions (Prop PairOfSmpl) (Int, Int) where
  sourceAbstractions =
    [ SoAs $
        SourceAbstraction
          { sourceAbsName = "pair"
          , constructor = (,)
          , productAbs =
              ( id @(ProductAbstraction (Prop IntSmpl) _ _ _) $
                  ProductAbstraction
                    { abstractionName = "L"
                    , propertyAbstraction = abstractsProperties (Prop . L . unProp)
                    , productModelAbstraction = _1
                    }
              )
                :& ProductAbstraction
                  { abstractionName = "R"
                  , propertyAbstraction = abstractsProperties (Prop . R . unProp)
                  , productModelAbstraction = _2
                  }
                :& Nil
          }
    ]

instance LogicalModel PairOfSmpl where
  logic = unProp <$> abstractionLogic

instance HasLogicalModel PairOfSmpl (Int, Int) where
  satisfiesProperty (L p) (x, _) = satisfiesProperty p x
  satisfiesProperty (R p) (_, x) = satisfiesProperty p x

instance HasPermutationGenerator (Prop PairOfSmpl) (Int, Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator (Prop PairOfSmpl) (Int, Int) where
  parameterisedGenerator = buildGen @(Prop PairOfSmpl)

instance Overlay (Prop PairSmpl) (Prop PairOfSmpl) (Int, Int) (Int, Int) where
  overlays (Prop BothNonNeg) = Prop <$> Var (L NonNegative) :&&: Var (R NonNegative)

instance HasLogicalModel PairSmpl (Int, Int) where
  satisfiesProperty = deduceFromOverlay . Prop

instance HasPermutationGenerator (Prop PairSmpl) (Int, Int) where
  sources = overlaySources

instance HasParameterisedGenerator (Prop PairSmpl) (Int, Int) where
  parameterisedGenerator = buildGen @(Prop PairSmpl)

smplPairTests :: TestTree
smplPairTests =
  testGroup
    "smplPairTests"
    [ fromGroup $ permutationGeneratorSelfTest @(Prop PairOfSmpl)
    , testProperty "overlay is sound" $ soundOverlay @(Prop PairSmpl)
    , fromGroup $ permutationGeneratorSelfTest @(Prop PairSmpl)
    ]
