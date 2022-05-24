module Spec.IntEither (
  IntEitherProp (..),
  intEitherGenTests,
) where

import Apropos
import Apropos.LogicalModel

import Control.Lens (_Left, _Right)
import Spec.IntPermutationGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntEitherProp
  = IsLeft
  | IsRight
  | L IntProp
  | R IntProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel IntEitherProp where
  logic =
    ExactlyOne [Var IsLeft, Var IsRight]
      :&&: fmap unProp (abstractionLogic @(Either Int Int))

instance HasLogicalModel IntEitherProp (Either Int Int) where
  satisfiesProperty IsLeft (Left _) = True
  satisfiesProperty IsLeft (Right _) = False
  satisfiesProperty IsRight (Right _) = True
  satisfiesProperty IsRight (Left _) = False
  satisfiesProperty (L _) (Right _) = False
  satisfiesProperty (L p) (Left m) = satisfiesProperty p m
  satisfiesProperty (R _) (Left _) = False
  satisfiesProperty (R p) (Right m) = satisfiesProperty p m

instance HasAbstractions (Prop IntEitherProp) (Either Int Int) where
  sumAbstractions =
    [ SuAs $
        id @(SumAbstraction (Prop IntProp) _ _ _) $
          SumAbstraction
            { abstractionName = "L"
            , propLabel = Prop IsLeft
            , sumModelAbstraction = _Left
            , propertyAbstraction = abstractsProperties (Prop . L . unProp)
            }
    , SuAs $
        SumAbstraction
          { abstractionName = "R"
          , propLabel = Prop IsRight
          , sumModelAbstraction = _Right
          , propertyAbstraction = abstractsProperties (Prop . R . unProp)
          }
    ]

instance HasPermutationGenerator (Prop IntEitherProp) (Either Int Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator (Prop IntEitherProp) (Either Int Int) where
  parameterisedGenerator = buildGen

intEitherGenTests :: TestTree
intEitherGenTests =
  testGroup "intPairGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              "Either Int Int Generator"
              (Yes @(Prop IntEitherProp))
          ]
