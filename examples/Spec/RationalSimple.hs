module Spec.RationalSimple (
  RatProp (..),
  ratSmplGenTests,
) where

import Apropos
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

-- This module is mostly just to test the Generator for rationals

data RatProp = Positive | Zero | Negative
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

instance LogicalModel RatProp where
  logic = ExactlyOne $ Var <$> [Positive, Zero, Negative]

instance HasLogicalModel RatProp Rational where
  satisfiesProperty Positive r = r > 0
  satisfiesProperty Negative r = r < 0
  satisfiesProperty Zero r = r == 0

instance HasPermutationGenerator RatProp Rational where
  sources =
    [ Source
        { sourceName = "zero"
        , covers = Var Zero
        , gen = pure 0
        }
    , Source
        { sourceName = "positive"
        , covers = Var Positive
        , gen = rational $ linear (1 / 1_000) 1_000_000
        }
    ]
  generators =
    [ Morphism
        { name = "negate"
        , match = Yes
        , contract = swap Positive Negative
        , morphism = pure . negate
        }
    ]

instance HasParameterisedGenerator RatProp Rational where
  parameterisedGenerator = buildGen

ratSmplGenTests :: TestTree
ratSmplGenTests =
  testGroup "simple rationals" $
    fromGroup
      <$> [ runGeneratorTestsWhere "gen tests" (Yes @RatProp)
          ]
