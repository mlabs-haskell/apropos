module Spec.TicTacToe.Player (
  PlayerProperty (..),
  playerPermutationGenSelfTest,
) where

import Apropos
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerProperty
  = PlayerIsX
  | PlayerIsO
  | PlayerIsInvalid
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Hashable)

instance Enumerable PlayerProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel PlayerProperty where
  logic = ExactlyOne $ Var <$> [PlayerIsInvalid, PlayerIsX, PlayerIsO]

instance HasLogicalModel PlayerProperty Int where
  satisfiesProperty PlayerIsX player = player == 1
  satisfiesProperty PlayerIsO player = player == 0
  satisfiesProperty PlayerIsInvalid player =
    not (satisfiesAny [PlayerIsX, PlayerIsO] player)

instance HasPermutationGenerator PlayerProperty Int where
  sources =
    [ Source
        { sourceName = "X"
        , covers = Var PlayerIsX
        , pgen = const $ pure 1
        }
    , Source
        { sourceName = "O"
        , covers = Var PlayerIsO
        , pgen = const $ pure 0
        }
    , Source
        { sourceName = "invalid"
        , covers = Var PlayerIsInvalid
        , pgen =
            const $
              genFilter (\i -> i `notElem` [0, 1]) $
                int (linear minBound maxBound)
        }
    ]

instance HasParameterisedGenerator PlayerProperty Int where
  parameterisedGenerator = buildGen

playerPermutationGenSelfTest :: TestTree
playerPermutationGenSelfTest =
  testGroup "playerPermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @PlayerProperty
