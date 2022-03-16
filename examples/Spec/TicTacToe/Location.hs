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
import GHC.Generics (Generic)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data LocationProperty
  = LocationIsWithinBounds
  | LocationIsOutOfBounds
  deriving stock (Eq, Ord, Enum, Show, Bounded,Generic)
  deriving anyclass Enumerable

instance LogicalModel LocationProperty where
  logic = ExactlyOne $ Var <$> [LocationIsWithinBounds, LocationIsOutOfBounds]

instance HasLogicalModel LocationProperty Int where
  satisfiesProperty LocationIsWithinBounds location = location >= 0 && location < 9
  satisfiesProperty LocationIsOutOfBounds location =
    not (satisfiesProperty LocationIsWithinBounds location)

instance HasPermutationGenerator LocationProperty Int where
  generators =
    [ Morphism
        { name = "MakeLocationIsWithinBounds"
        , match = Var LocationIsOutOfBounds
        , contract = remove LocationIsOutOfBounds >> add LocationIsWithinBounds
        , morphism = \_ -> int (linear 0 8)
        }
    , Morphism
        { name = "MakeLocationIsOutOfBounds"
        , match = Var LocationIsWithinBounds
        , contract = remove LocationIsWithinBounds >> add LocationIsOutOfBounds
        , morphism = \_ ->
            choice
              [ int (linear minBound (-1))
              , int (linear 9 maxBound)
              ]
        }
    ]

instance HasParameterisedGenerator LocationProperty Int where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Int
baseGen = int (linear minBound maxBound)

locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest =
  testGroup "locationPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism LocationProperty Int) -> True)
        baseGen
