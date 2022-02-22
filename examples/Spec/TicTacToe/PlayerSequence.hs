module Spec.TicTacToe.PlayerSequence (
  PlayerSequenceProperty(..),
  playerSequencePermutationGenSelfTest,
  ) where
import Brutus.HasLogicalModel
import Brutus.LogicalModel
import Brutus.HasParameterisedGenerator
import Brutus.HasPermutationGenerator
import Brutus.HasPermutationGenerator.Contract
import Brutus.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear,singleton)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data PlayerSequenceProperty =
    TakeTurns
  | Don'tTakeTurns
  | PlayerSequenceNull
  | PlayerSequenceSingleton
  | PlayerSequenceIsLongerThanGame
  deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Enumerable PlayerSequenceProperty where
  enumerated = [minBound..maxBound]

instance LogicalModel PlayerSequenceProperty where
  logic = (ExactlyOne $ Var <$> [TakeTurns,Don'tTakeTurns])
       :&&: (Var PlayerSequenceNull :->: Var TakeTurns)
       :&&: (AtMostOne [Var PlayerSequenceNull,Var PlayerSequenceSingleton,Var PlayerSequenceIsLongerThanGame])


playersTakeTurns :: [Int] -> Bool
playersTakeTurns playerSeq =
  let turn 1 0 = True
      turn 0 1 = True
      turn _ _ = False
      go [] = True
      go [p] = p `elem` [0,1]
      go (m0:m1:mrest) = turn m0 m1 && go (m1:mrest)
    in go playerSeq

instance HasLogicalModel PlayerSequenceProperty [Int] where
  satisfiesProperty TakeTurns m = playersTakeTurns m
  satisfiesProperty Don'tTakeTurns m =
    not (satisfiesProperty TakeTurns m)
  satisfiesProperty PlayerSequenceNull m = length m == 0
  satisfiesProperty PlayerSequenceSingleton m = length m == 1
  satisfiesProperty PlayerSequenceIsLongerThanGame m = length m > 9

instance HasPermutationGenerator PlayerSequenceProperty [Int] where
  generators =
    [ PermutationEdge
      { name = "MakeTakeTurnsNotLongerThanGame"
      , match = Yes
      , contract = removeAll [Don'tTakeTurns
                             ,PlayerSequenceSingleton
                             ,PlayerSequenceNull
                             ,PlayerSequenceIsLongerThanGame
                             ] >> add TakeTurns
      , permuteGen = do
          s <- source
          let numMoves = min 9 (max 2 (length s))
          pattern <- liftGenPA $ Gen.element [[0,1],[1,0]]
          pure $ take numMoves (cycle pattern)
      }
    , PermutationEdge
      { name = "MakeTakeTurnsLongerThanGame"
      , match = Yes
      , contract = removeAll [Don'tTakeTurns
                             ,PlayerSequenceSingleton
                             ,PlayerSequenceNull]
                   >> addAll [TakeTurns
                             ,PlayerSequenceIsLongerThanGame]
      , permuteGen = do
          let numMoves = 10
          pattern <- liftGenPA $ Gen.element [[0,1],[1,0]]
          pure $ take numMoves (cycle pattern)
      }
    , PermutationEdge
      { name = "MakePlayerSequenceNull"
      , match = Yes
      , contract = removeAll [Don'tTakeTurns,PlayerSequenceSingleton]
                 >> addAll [TakeTurns,PlayerSequenceNull]
      , permuteGen = pure []
      }
    , PermutationEdge
      { name = "MakePlayerSingletonDon'tTakeTurns"
      , match = Yes
      , contract = removeAll [TakeTurns,PlayerSequenceNull]
             >> addAll [Don'tTakeTurns,PlayerSequenceSingleton]
      , permuteGen = do
          liftGenPA $ Gen.list (singleton 1)
                    $ Gen.choice [Gen.int (linear minBound (-1))
                                 ,Gen.int (linear 2 maxBound)
                                 ]
      }
    , PermutationEdge
      { name = "MakePlayerSingletonTakeTurns"
      , match = Yes
      , contract = removeAll [Don'tTakeTurns,PlayerSequenceNull]
               >> addAll [TakeTurns,PlayerSequenceSingleton]
      , permuteGen = do
          liftGenPA $ Gen.list (singleton 1) $ Gen.int (linear 0 1)
      }
    , PermutationEdge
      { name = "MakeDon'tTakeTurns"
      , match = Yes
      , contract = add Don'tTakeTurns
            >> removeAll [TakeTurns,PlayerSequenceSingleton,PlayerSequenceNull]
      , permuteGen = do
          s <- source
          let numMoves = max 2 (length s)
          let genFoulPlay = do
                Gen.filter (satisfiesProperty Don'tTakeTurns)
                         $ Gen.list (singleton numMoves)
                         $ Gen.int (linear 0 1)
              genInvalid = do
                Gen.filter (satisfiesProperty Don'tTakeTurns)
                         $ Gen.list (singleton numMoves)
                         $ Gen.int (linear minBound maxBound)
          liftGenPA $ Gen.choice [genFoulPlay,genInvalid]

      }
    ]

instance HasParameterisedGenerator PlayerSequenceProperty [Int] where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen [Int]
baseGen = liftGenP $ Gen.list (linear 0 10) $ Gen.int (linear minBound maxBound)

playerSequencePermutationGenSelfTest :: TestTree
playerSequencePermutationGenSelfTest = testGroup "movePermutationGenSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest True
             (\(_ :: PermutationEdge PlayerSequenceProperty [Int]) -> True)
             baseGen


