module Spec.TicTacToe.Location (
  LocationProperty (..),
  locationPermutationGenSelfTest,
) where

import Apropos
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

instance HasPermutationGenerator LocationProperty Int where
  sources =
    [ Source
        { sourceName = "in bounds"
        , covers = Var LocationIsWithinBounds
        , gen = int (linear 0 8)
        }
    , Source
        { sourceName = "out of bounds"
        , covers = Var LocationIsOutOfBounds
        , gen =
            choice
              [ int (linear minBound (-1))
              , int (linear 9 maxBound)
              ]
        }
    ]

instance HasParameterisedGenerator LocationProperty Int where
  parameterisedGenerator = buildGen

locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest =
  testGroup "locationPermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @LocationProperty
