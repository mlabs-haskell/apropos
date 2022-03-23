{- |
Module: Spec.TicTacToe.Player
Description: TicTacToe player properties and model.
-}
module Spec.TicTacToe.Player (
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

-- | Properties which may be true of a TicTacToe player.
data PlayerProperty
  = -- | Player is playing crosses.
    PlayerIsX
  | -- | Player is playing noughts.
    PlayerIsO
  | -- | Player is invalid.
    PlayerIsInvalid
  deriving stock (Eq, Ord, Enum, Show, Bounded)

instance Enumerable PlayerProperty where
  enumerated = [minBound .. maxBound]

-- | Relationships between `PlayerProperty`s.
instance LogicalModel PlayerProperty where
  logic = ExactlyOne $ Var <$> [PlayerIsInvalid, PlayerIsX, PlayerIsO]

-- | The properties of a player may be defined with `Int`s.
instance HasLogicalModel PlayerProperty Int where
  -- Player is crosses if player :: Int == 1.
  satisfiesProperty PlayerIsX player = player == 1
  -- Player is noughts if player :: Int == 0.
  satisfiesProperty PlayerIsO player = player == 0
  -- All other values of player :: Int are considered invalid.
  satisfiesProperty PlayerIsInvalid player =
    not (satisfiesAny [PlayerIsX, PlayerIsO] player)

{- | Any player :: `Int` can be transformed into a different type
     of player.
-}
instance HasPermutationGenerator PlayerProperty Int where
  generators =
    [ Morphism
        { -- To make a player crosses, set their value to 1.
          name = "MakePlayerX"
        , match = Not $ Var PlayerIsX
        , contract = removeAll [PlayerIsO, PlayerIsInvalid] >> add PlayerIsX
        , morphism = \_ -> pure 1
        }
    , Morphism
        { -- To make a player noughts, set their value to 0.
          name = "MakePlayerO"
        , match = Not $ Var PlayerIsO
        , contract = removeAll [PlayerIsX, PlayerIsInvalid] >> add PlayerIsO
        , morphism = \_ -> pure 0
        }
    , Morphism
        { -- To make a player invalid, set their value to anything
          -- other than zero or one.
          name = "MakePlayerInvalid"
        , match = Not $ Var PlayerIsInvalid
        , contract = removeAll [PlayerIsX, PlayerIsO] >> add PlayerIsInvalid
        , morphism = \_ ->
            genFilter (\i -> i `notElem` [0, 1]) $
              int (linear minBound maxBound)
        }
    ]

instance HasParameterisedGenerator PlayerProperty Int where
  parameterisedGenerator = buildGen baseGen

-- | Generator for all `Int`s.
baseGen :: Gen Int
baseGen = int (linear minBound maxBound)

-- | Tests ensuring that the model is sound.
playerPermutationGenSelfTest :: TestTree
playerPermutationGenSelfTest =
  testGroup "playerPermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism PlayerProperty Int) -> True)
        baseGen
