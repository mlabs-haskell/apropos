{- |
Module: Spec.TicTacToe.Location
Description: Defines logical properties of locations on a TicTacToe grid.
-}
module Spec.TicTacToe.Location (
  LocationProperty (..),
  locationPermutationGenSelfTest,
  Location,
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

{- | Type synonym defining a location on a TicTacToe grid as an Int.
     Zero through eight are valid squares, all others are invalid.
-}
type Location = Int

-- | Properties of a "Location" (or grid space) in a game of TicTacToe.
data LocationProperty
  = -- | Proposed location is inside 3x3 grid.
    LocationIsWithinBounds
  | -- | Proposed locations is outside of 3x3 grid.
    LocationIsOutOfBounds
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel LocationProperty where
  -- Only relation between the two 'LocationProperty's is that
  -- they may not both hold at once.
  logic = ExactlyOne $ Var <$> [LocationIsWithinBounds, LocationIsOutOfBounds]

{- | TicTacToe grid spaces may be conceived of as an integer
     between 0 and 8.
-}
instance HasLogicalModel LocationProperty Int where
  satisfiesProperty LocationIsWithinBounds location = location >= 0 && location < 9
  satisfiesProperty LocationIsOutOfBounds location =
    not (satisfiesProperty LocationIsWithinBounds location)

{- | Any space out of bounds may be altered to be within
     bounds and vice versa.
-}
instance HasPermutationGenerator LocationProperty Int where
  generators =
    [ Morphism
        { name = "MakeLocationIsWithinBounds"
        , match = Var LocationIsOutOfBounds
        , contract = remove LocationIsOutOfBounds >> add LocationIsWithinBounds
        , -- Whatever the value is, replace it with a number between
          -- zero and eight.
          morphism = \_ -> int (linear 0 8)
        }
    , Morphism
        { name = "MakeLocationIsOutOfBounds"
        , match = Var LocationIsWithinBounds
        , contract = remove LocationIsWithinBounds >> add LocationIsOutOfBounds
        , -- Replace the number with some value between `minBound` and -1
          -- or 9 and `maxBound`.
          morphism = \_ ->
            choice
              [ int (linear minBound (-1))
              , int (linear 9 maxBound)
              ]
        }
    ]

instance HasParameterisedGenerator LocationProperty Int where
  parameterisedGenerator = buildGen baseGen

{- | A generator for any `Int` between minBound and maxBound
     i.e. all `Int`s.
-}
baseGen :: Gen Int
baseGen = int (linear minBound maxBound)

{- | Test that ensures all valid property combinations can
     be generated successfully.
-}
locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest =
  testGroup "locationPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism LocationProperty Int) -> True)
        baseGen
