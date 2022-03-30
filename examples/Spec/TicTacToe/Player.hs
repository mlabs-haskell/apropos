module Spec.TicTacToe.Player (
  Player (..),
  PlayerProperty (..),
  playerPermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data Player = X | O
  deriving stock (Eq, Show)

data PlayerProperty
  = PlayerIsX
  | PlayerIsO
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable PlayerProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel PlayerProperty where
  logic = ExactlyOne $ Var <$> [PlayerIsX, PlayerIsO]

instance HasLogicalModel PlayerProperty Player where
  satisfiesProperty PlayerIsX player = player == X
  satisfiesProperty PlayerIsO player = player == O

instance HasPermutationGenerator PlayerProperty Player where
  generators =
    [ Morphism
        { name = "MakePlayerX"
        , match = Not $ Var PlayerIsX
        , contract = remove PlayerIsO >> add PlayerIsX
        , morphism = \_ -> pure X
        }
    , Morphism
        { name = "MakePlayerO"
        , match = Not $ Var PlayerIsO
        , contract = remove PlayerIsX >> add PlayerIsO
        , morphism = \_ -> pure O
        }
    ]

instance HasParameterisedGenerator PlayerProperty Player where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen Player
baseGen = element [X, O]

playerPermutationGenSelfTest :: TestTree
playerPermutationGenSelfTest =
  testGroup "playerPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism PlayerProperty Player) -> True)
        baseGen
