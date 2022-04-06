module Spec.TicTacToe.PlayerSequence (
  PlayerSequenceProperty (..),
  playerSequencePermutationGenSelfTest,
) where

import Apropos.Gen
import Apropos.HasLogicalModel
import Apropos.HasParameterisedGenerator
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.LogicalModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data PlayerSequenceProperty
  = TakeTurns
  | Don'tTakeTurns
  | PlayerSequenceNull
  | PlayerSequenceSingleton
  | PlayerSequenceIsLongerThanGame
  deriving stock (Eq, Ord, Enum, Show, Bounded, Generic)
  deriving anyclass (Hashable)

instance Enumerable PlayerSequenceProperty where
  enumerated = [minBound .. maxBound]

instance LogicalModel PlayerSequenceProperty where
  logic =
    ExactlyOne (Var <$> [TakeTurns, Don'tTakeTurns])
      :&&: (Var PlayerSequenceNull :->: Var TakeTurns)
      :&&: AtMostOne [Var PlayerSequenceNull, Var PlayerSequenceSingleton, Var PlayerSequenceIsLongerThanGame]

playersTakeTurns :: [Int] -> Bool
playersTakeTurns playerSeq =
  let turn 1 0 = True
      turn 0 1 = True
      turn _ _ = False
      go [] = True
      go [p] = p `elem` [0, 1]
      go (m0 : m1 : mrest) = turn m0 m1 && go (m1 : mrest)
   in go playerSeq

instance HasLogicalModel PlayerSequenceProperty [Int] where
  satisfiesProperty TakeTurns m = playersTakeTurns m
  satisfiesProperty Don'tTakeTurns m =
    not (satisfiesProperty TakeTurns m)
  satisfiesProperty PlayerSequenceNull m = null m
  satisfiesProperty PlayerSequenceSingleton m = length m == 1
  satisfiesProperty PlayerSequenceIsLongerThanGame m = length m > 9

instance HasPermutationGenerator PlayerSequenceProperty [Int] where
  generators =
    [ Morphism
        { name = "MakeTakeTurnsNotLongerThanGame"
        , match = Yes
        , contract =
            removeAll
              [ Don'tTakeTurns
              , PlayerSequenceSingleton
              , PlayerSequenceNull
              , PlayerSequenceIsLongerThanGame
              ]
              >> add TakeTurns
        , morphism = \s -> do
            let numMoves = min 9 (max 2 (length s))
            pat <- element [[0, 1], [1, 0]]
            pure $ take numMoves (cycle pat)
        }
    , Morphism
        { name = "MakeTakeTurnsLongerThanGame"
        , match = Yes
        , contract =
            removeAll
              [ Don'tTakeTurns
              , PlayerSequenceSingleton
              , PlayerSequenceNull
              ]
              >> addAll
                [ TakeTurns
                , PlayerSequenceIsLongerThanGame
                ]
        , morphism = \_ -> do
            let numMoves = 10
            pat <- element [[0, 1], [1, 0]]
            pure $ take numMoves (cycle pat)
        }
    , Morphism
        { name = "MakePlayerSequenceNull"
        , match = Yes
        , contract =
            removeAll [Don'tTakeTurns, PlayerSequenceSingleton]
              >> addAll [TakeTurns, PlayerSequenceNull]
        , morphism = \_ -> pure []
        }
    , Morphism
        { name = "MakePlayerSingletonDon'tTakeTurns"
        , match = Yes
        , contract =
            removeAll [TakeTurns, PlayerSequenceNull]
              >> addAll [Don'tTakeTurns, PlayerSequenceSingleton]
        , morphism = \_ ->
            list (singleton 1) $
              choice
                [ int (linear minBound (-1))
                , int (linear 2 maxBound)
                ]
        }
    , Morphism
        { name = "MakePlayerSingletonTakeTurns"
        , match = Yes
        , contract =
            removeAll [Don'tTakeTurns, PlayerSequenceNull]
              >> addAll [TakeTurns, PlayerSequenceSingleton]
        , morphism = \_ -> list (singleton 1) $ int (linear 0 1)
        }
    , Morphism
        { name = "MakeDon'tTakeTurns"
        , match = Yes
        , contract =
            add Don'tTakeTurns
              >> removeAll [TakeTurns, PlayerSequenceSingleton, PlayerSequenceNull]
        , morphism = \s -> do
            let numMoves = max 2 (length s)
            let genFoulPlay = do
                  genFilter (satisfiesProperty Don'tTakeTurns) $
                    list (singleton numMoves) $
                      int (linear 0 1)
                genInvalid = do
                  genFilter (satisfiesProperty Don'tTakeTurns) $
                    list (singleton numMoves) $
                      int (linear minBound maxBound)
            choice [genFoulPlay, genInvalid]
        }
    ]

instance HasParameterisedGenerator PlayerSequenceProperty [Int] where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen [Int]
baseGen = list (linear 0 10) $ int (linear minBound maxBound)

playerSequencePermutationGenSelfTest :: TestTree
playerSequencePermutationGenSelfTest =
  testGroup "playerSequencePermutationGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism PlayerSequenceProperty [Int]) -> True)
        baseGen
