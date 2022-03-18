module Spec.IntEither(
  IntEitherProp(..),
  intEitherGenTests,
  ) where

import Apropos
import GHC.Generics ( Generic )
import Spec.IntPermutationGen
import Control.Lens ( _Left, _Right )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntEitherProp
  = IsLeft
  | IsRight
  | L IntProp
  | R IntProp
  deriving stock (Eq,Ord,Show,Generic)
  deriving anyclass Enumerable

instance LogicalModel IntEitherProp where
  logic =
    ExactlyOne [ Var IsLeft , Var IsRight ]
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

instance HasPermutationGenerator IntEitherProp (Either Int Int) where
  generators =
    let l =
          SumAbstraction
            { abstractionName = "L"
            , propertyAbstraction = abstractsProperties L
            , abstractionMatch = Var IsLeft
            , sumModelAbstraction = _Left
            }
        r =
          SumAbstraction
            { abstractionName = "R"
            , propertyAbstraction = abstractsProperties R
            , abstractionMatch = Var IsRight
            , sumModelAbstraction = _Right
            }
      in
        [ Morphism
          { name = "make left"
          , match = Var IsRight
          , contract = remove IsRight >> removeAll (R <$> enumerated) >> add IsLeft >> addAll (L <$> satisfiedBy)
          , morphism = const $ Left <$> genSatisfying @IntProp (All (Var <$> satisfiedBy))
          }
        , Morphism
          { name = "make right"
          , match = Var IsLeft
          , contract = remove IsLeft >> removeAll (L <$> enumerated) >> add IsRight >> addAll (R <$> satisfiedBy)
          , morphism = const $ Right <$> genSatisfying @IntProp (All (Var <$> satisfiedBy))
          }
        ]
        ++ (abstract l <$> generators)
        ++ (abstract r <$> generators)


instance HasParameterisedGenerator IntEitherProp (Either Int Int) where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen (Either Int Int)
baseGen = choice
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
