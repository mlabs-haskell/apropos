module Spec.TicTacToe.Location (
  LocationProperty (..),
  locationPermutationGenSelfTest,
) where

import Apropos
import Apropos.LogicalModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)


data LocationProperty
  = LocationIsWithinBounds
  | LocationIsOutOfBounds
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable, Hashable)

instance LogicalModel LocationProperty where
  logic = ExactlyOne $ Var <$> [LocationIsWithinBounds, LocationIsOutOfBounds]

instance HasLogicalModel LocationProperty Int where
  satisfiesProperty LocationIsWithinBounds location = location >= 0 && location < 9
  satisfiesProperty LocationIsOutOfBounds location =
    not (satisfiesProperty LocationIsWithinBounds location)

instance HasPermutationGenerator (Prop LocationProperty) Int where
  sources =
    [ Source
        { sourceName = "in bounds"
        , covers = Var (Prop LocationIsWithinBounds)
        , gen = int (linear 0 8)
        }
    , Source
        { sourceName = "out of bounds"
        , covers = Var (Prop LocationIsOutOfBounds)
        , gen =
            choice
              [ int (linear minBound (-1))
              , int (linear 9 maxBound)
              ]
        }
    ]

instance HasParameterisedGenerator (Prop LocationProperty) Int where
  parameterisedGenerator = buildGen @(Prop LocationProperty)

locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest =
  testGroup "locationPermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop LocationProperty)
