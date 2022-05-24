module Spec.TicTacToe.PlayerSequence (
  PlayerSequenceProperty (..),
  playerSequencePermutationGenSelfTest,
) where

import Apropos
import Apropos.LogicalModel
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerSequenceProperty
  = TakeTurns
  | Don'tTakeTurns
  | PlayerSequenceNull
  | PlayerSequenceSingleton
  | PlayerSequenceIsLongerThanGame
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Enumerable)

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

instance HasPermutationGenerator (Prop PlayerSequenceProperty) [Int] where
  sources =
    [ Source
        { sourceName = "player singleton take turns"
        , covers = Var (Prop TakeTurns) :&&: Var (Prop PlayerSequenceSingleton)
        , gen = list (singleton 1) $ int (linear 0 1)
        }
    , Source
        { sourceName = "player singleton don't take turns"
        , covers = Var (Prop Don'tTakeTurns) :&&: Var (Prop PlayerSequenceSingleton)
        , gen =
            list (singleton 1) $
              choice
                [ int (linear minBound (-1))
                , int (linear 2 maxBound)
                ]
        }
    , Source
        { sourceName = "null"
        , covers = Var (Prop PlayerSequenceNull)
        , gen = pure []
        }
    , Source
        { sourceName = "turns longer than name"
        , covers = Var (Prop PlayerSequenceIsLongerThanGame) :&&: Var (Prop TakeTurns)
        , gen = do
            let numMoves = 10
            pat <- element [[0, 1], [1, 0]]
            pure $ take numMoves (cycle pat)
        }
    ]
  generators =
    [ Morphism
        { name = "MakeTakeTurnsNotLongerThanGame"
        , match = Yes
        , contract =
            removeAll (map Prop
              [ Don'tTakeTurns
              , PlayerSequenceSingleton
              , PlayerSequenceNull
              , PlayerSequenceIsLongerThanGame
              ])
              >> add (Prop TakeTurns)
        , morphism = \s -> do
            let numMoves = min 9 (max 2 (length s))
            pat <- element [[0, 1], [1, 0]]
            pure $ take numMoves (cycle pat)
        }
    , Morphism
        { name = "MakeDon'tTakeTurns"
        , match = Yes
        , contract =
            add (Prop Don'tTakeTurns)
              >> removeAll (map Prop [TakeTurns, PlayerSequenceSingleton, PlayerSequenceNull])
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

instance HasParameterisedGenerator (Prop PlayerSequenceProperty) [Int] where
  parameterisedGenerator = buildGen

playerSequencePermutationGenSelfTest :: TestTree
playerSequencePermutationGenSelfTest =
  testGroup "playerSequencePermutationGenSelfTest" $
    pure $
      fromGroup $
        permutationGeneratorSelfTest @(Prop PlayerSequenceProperty)
