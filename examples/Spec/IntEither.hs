module Spec.IntEither (
  IntEitherProp (..),
  intEitherGenTests,
) where

import Apropos
import Control.Lens (_Left, _Right)
import GHC.Generics (Generic)
import Spec.IntPermutationGen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntEitherProp
  = IsLeft
  | IsRight
  | L IntProp
  | R IntProp
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel IntEitherProp where
  logic =
    ExactlyOne [Var IsLeft, Var IsRight]
      :&&: (Var IsLeft :->: (L <$> logic) :&&: None (Var . R <$> enumerated))
      :&&: (Var IsRight :->: (R <$> logic) :&&: None (Var . L <$> enumerated))

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
  abstractions =
    [ WrapAbs $
        SumAbstraction
          { abstractionName = "L"
          , propertyAbstraction = abstractsProperties L
          , propLabel = IsLeft
          , sumModelAbstraction = _Left
          , propConstructor = L
          , modelConstructor = Left
          }
    , WrapAbs $
        SumAbstraction
          { abstractionName = "R"
          , propertyAbstraction = abstractsProperties R
          , propLabel = IsRight
          , sumModelAbstraction = _Right
          , modelConstructor = Right
          , propConstructor = R
          }
    ]

instance HasPermutationGenerator IntEitherProp (Either Int Int) where
  generators = abstractionGenerators

instance HasParameterisedGenerator IntEitherProp (Either Int Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen (Either Int Int)
baseGen =
  choice
    [ Left <$> genSatisfying @IntProp Yes
    , Right <$> genSatisfying @IntProp Yes
    ]

intEitherGenTests :: TestTree
intEitherGenTests =
  testGroup "intPairGenTests" $
    fromGroup
      <$> [ runGeneratorTestsWhere
              (Apropos :: Either Int Int :+ IntEitherProp)
              "(Int,Int) Generator"
              Yes
          ]
