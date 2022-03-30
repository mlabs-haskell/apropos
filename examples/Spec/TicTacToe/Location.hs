module Spec.TicTacToe.Location (
  Location (..),
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

data Location = Location
  { row :: Int
  , column :: Int
  }
  deriving stock (Eq, Ord, Show)

data LocationProperty
  = LocationIsWithinBounds
  | LocationIsOutOfBounds
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Enumerable)

instance LogicalModel LocationProperty where
  logic = ExactlyOne $ Var <$> [LocationIsWithinBounds, LocationIsOutOfBounds]

instance HasLogicalModel LocationProperty Location where
  satisfiesProperty LocationIsWithinBounds location =
    and
      [ row location >= 0
      , row location < 3
      , column location >= 0
      , column location < 3
      ]
  satisfiesProperty LocationIsOutOfBounds location =
    not (satisfiesProperty LocationIsWithinBounds location)

instance HasPermutationGenerator LocationProperty Location where
  generators =
    [ Morphism
        { name = "MakeLocationIsWithinBounds"
        , match = Var LocationIsOutOfBounds
        , contract = remove LocationIsOutOfBounds >> add LocationIsWithinBounds
        , morphism = \_ -> do
            x <- int (linear 0 2)
            y <- int (linear 0 2)
            return $
              Location
                { row = x
                , column = y
                }
        }
    , Morphism
        { name = "MakeLocationIsOutOfBounds"
        , match = Var LocationIsWithinBounds
        , contract = remove LocationIsWithinBounds >> add LocationIsOutOfBounds
        , morphism = \location ->
            choice
              [ do
                  x <- int (linear minBound (-1))
                  return $ location {row = x}
              , do
                  x <- int (linear 3 maxBound)
                  return $ location {row = x}
              , do
                  y <- int (linear minBound (-1))
                  return $ location {column = y}
              , do
                  y <- int (linear 3 maxBound)
                  return $ location {column = y}
              ]
        }
    ]

instance HasParameterisedGenerator LocationProperty Location where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Location
baseGen = do
  x <- int (linear minBound maxBound)
  y <- int (linear minBound maxBound)
  return $
    Location
      { row = x
      , column = y
      }

locationPermutationGenSelfTest :: TestTree
locationPermutationGenSelfTest =
  testGroup "locationPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism LocationProperty Location) -> True)
        baseGen
