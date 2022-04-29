module Spec.IntEither (
  IntEitherProp (..),
  intEitherGenTests,
) where

import Apropos
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
      :&&: abstractionLogic @(Either Int Int)

instance HasLogicalModel IntEitherProp (Either Int Int) where
  satisfiesProperty IsLeft (Left _) = True
  satisfiesProperty IsLeft (Right _) = False
  satisfiesProperty IsRight (Right _) = True
  satisfiesProperty IsRight (Left _) = False
  satisfiesProperty (L _) (Right _) = False
  satisfiesProperty (L p) (Left m) = satisfiesProperty p m
  satisfiesProperty (R _) (Left _) = False
  satisfiesProperty (R p) (Right m) = satisfiesProperty p m

instance HasAbstractions IntEitherProp (Either Int Int) where
  sumAbstractions =
    [ SuAs $
        SumAbstraction
          { abstractionName = "L"
          , propLabel = IsLeft
          , sumModelAbstraction = _Left
          , propertyAbstraction = abstractsProperties L
          }
    , SuAs $
        SumAbstraction
          { abstractionName = "R"
          , propLabel = IsRight
          , sumModelAbstraction = _Right
          , propertyAbstraction = abstractsProperties R
          }
    ]

instance HasPermutationGenerator IntEitherProp (Either Int Int) where
  sources = abstractionSources
  generators = abstractionMorphisms

instance HasParameterisedGenerator IntEitherProp (Either Int Int) where
  parameterisedGenerator = buildGen -- baseGen

intEitherGenTests :: TestTree
intEitherGenTests =
  testGroup "intPairGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              (Apropos :: Either Int Int :+ IntEitherProp)
              "Either Int Int Generator"
              Yes
          ]
