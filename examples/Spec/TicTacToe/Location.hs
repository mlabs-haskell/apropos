module Spec.TicTacToe.Location (
  LocationProperty (..),
  locationPermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data LocationProperty
  = LocationIsWithinBounds
  | LocationIsOutOfBounds
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable LocationProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel LocationProperty where
  logic = (ExactlyOne $ Var <$> [LocationIsWithinBounds, LocationIsOutOfBounds])

instance HasLogicalModel LocationProperty Int where
  satisfiesProperty LocationIsWithinBounds location = location >= 0 && location < 9
  satisfiesProperty LocationIsOutOfBounds location =
    not (satisfiesProperty LocationIsWithinBounds location)

instance HasPermutationGenerator LocationProperty Int where
  generators =
    [ PermutationEdge
        { name = "MakeLocationIsWithinBounds"
        , match = Var LocationIsOutOfBounds
        , contract = remove LocationIsOutOfBounds >> add LocationIsWithinBounds
        , permuteGen = int (linear 0 8)
        }
    , PermutationEdge
        { name = "MakeLocationIsOutOfBounds"
        , match = Var LocationIsWithinBounds
        , contract = remove LocationIsWithinBounds >> add LocationIsOutOfBounds
        , permuteGen =
            choice $
                [ int (linear minBound (-1))
                , int (linear 9 maxBound)
                ]
        }
    ]

instance HasParameterisedGenerator LocationProperty Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen' Int
baseGen = int (linear minBound maxBound)

locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest =
  testGroup "locationPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: PermutationEdge LocationProperty Int) -> True)
        baseGen
