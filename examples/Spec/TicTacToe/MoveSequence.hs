{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe.MoveSequence (
  moveSequencePermutationGenSelfTest
  ) where
import Spec.TicTacToe.PlayerSequence
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerLocationSequencePair
import Apropos.HasLogicalModel
import Apropos.LogicalModel
import Apropos.HasPermutationGenerator
import Apropos.HasPermutationGenerator.Contract
import Apropos.HasParameterisedGenerator
import Apropos.Gen
--import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
--import Control.Monad (join)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Reader (ask)
--import Control.Monad.Trans (lift)

data MoveSequenceProperty =
    MoveSequenceValid
  | MoveSequenceContainsWinForX
  | MoveSequenceContainsWinForO
  | MoveSequenceLength Int
  deriving stock (Eq,Ord,Show)

instance Enumerable MoveSequenceProperty where
  enumerated = [MoveSequenceValid,MoveSequenceContainsWinForX,MoveSequenceContainsWinForO]
            <> [MoveSequenceLength l | l <- [0..10]]

isWinFor :: Int -> [(Int,Int)] -> Bool
isWinFor p ms =
  let locations = Set.fromList (snd <$> (filter ((==p) . fst) ms))
   in any (`Set.isSubsetOf` locations) winTileSets

winTileSets :: [Set Int]
winTileSets = Set.fromList <$> [[0,1,2],[3,4,5],[6,7,8]
                               ,[0,3,6],[1,4,7],[2,5,8]
                               ,[0,4,8],[2,4,6]
                               ]

instance LogicalModel MoveSequenceProperty where
  logic = All
    [ Var MoveSequenceValid :->: (AtMostOne $ Var <$> [MoveSequenceContainsWinForX
                                                      ,MoveSequenceContainsWinForO])
    , Some (Var <$> [MoveSequenceLength l | l <- [0..4]])
         :->: Not (Var MoveSequenceContainsWinForX :||: Var MoveSequenceContainsWinForO)
    , ExactlyOne (Var <$> [MoveSequenceLength l | l <- [0..10]])
    , Var (MoveSequenceLength 10) :->: (All $ Not <$> [Var MoveSequenceValid
                                                      ,Var MoveSequenceContainsWinForX
                                                      ,Var MoveSequenceContainsWinForO
                                                      ])
    , Var (MoveSequenceLength 0) :->: Var MoveSequenceValid
    ]

playPiece :: Contract MoveSequenceProperty ()
playPiece = (Set.map incrementLength <$> readContractOutput) >>= setContractOutput
  where incrementLength (MoveSequenceLength i) = MoveSequenceLength (i + 1)
        incrementLength p = p

playerSequenceIsValid :: Formula PlayerSequenceProperty
playerSequenceIsValid = Var TakeTurns :&&: (Not $ Var PlayerSequenceIsLongerThanGame)

locationSequenceIsValid :: Formula LocationSequenceProperty
locationSequenceIsValid = Var AllLocationsAreInBounds
                     :&&: (Not $ Var SomeLocationIsOccupiedTwice)

instance HasLogicalModel MoveSequenceProperty [(Int,Int)] where
  satisfiesProperty MoveSequenceContainsWinForX ms = isWinFor 1 ms
  satisfiesProperty MoveSequenceContainsWinForO ms = isWinFor 0 ms
  satisfiesProperty MoveSequenceValid ms =
    (not (isWinFor 1 ms && isWinFor 0 ms))
      && satisfiesExpression playerSequenceIsValid (fst <$> ms)
      && satisfiesExpression locationSequenceIsValid (snd <$> ms)
  satisfiesProperty (MoveSequenceLength l) ms = length ms == l

baseGen :: PGen [(Int,Int)]
baseGen = do
  pl <- genSatisfying (Var PlayerLocationSequencePairLengthsAreEqual)
  pure $ (uncurry zip) pl

instance HasPermutationGenerator MoveSequenceProperty [(Int,Int)] where
  generators =
    [ PermutationEdge
      { name = "Empty"
      , match = Yes
      , contract = clear >> addAll [ MoveSequenceValid, MoveSequenceLength 0]
      , permuteGen = pure []
      }
    , PermutationEdge
      { name = "PlaceInvalidLocation"
      , match = Yes
      , contract = playPiece >> remove MoveSequenceValid
      , permuteGen = do
          moves <- ask
          --TODO randomly generate random location
          --TODO use valid player
          pure $ (0,-100):moves
      }
    , PermutationEdge
      { name = "PlaceInvalidPlayer"
      , match = Yes
      , contract = playPiece >> remove MoveSequenceValid
      , permuteGen = do
          moves <- ask
          --TODO find valid location
          --TODO generate invalid player
          pure $ (0,-100):moves
      }
    , PermutationEdge
      { name = "PlaceValidNoWin"
      , match = Yes
      , contract = removeIf (MoveSequenceLength 9) MoveSequenceValid >> playPiece
      , permuteGen = do
          moves <- ask
          --TODO find valid location
          --TODO use valid player
          pure $ (0,0):moves
      }
    , PermutationEdge
      { name = "PlaceWinPlayerO"
      , match = None $ Var <$> [MoveSequenceLength l | l <- [0..3]]
      , contract = removeIf (MoveSequenceLength 9) MoveSequenceValid >> playPiece >> add MoveSequenceContainsWinForO
      , permuteGen = do
          moves <- ask
          --TODO find valid location
          --TODO use valid player
          pure $ (0,0):moves
      }
    , PermutationEdge
      { name = "PlaceWinPlayerX"
      , match = None $ Var <$> [MoveSequenceLength l | l <- [0..3]]
      , contract = removeIf (MoveSequenceLength 9) MoveSequenceValid >> playPiece >> add MoveSequenceContainsWinForX
      , permuteGen = do
          moves <- ask
          --TODO find valid location
          --TODO use valid player
          pure $ (0,0):moves
      }
    ]

instance HasParameterisedGenerator MoveSequenceProperty [(Int,Int)] where
  parameterisedGenerator = buildGen baseGen

moveSequencePermutationGenSelfTest :: TestTree
moveSequencePermutationGenSelfTest = testGroup "moveSequencePermutationGenSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest
                 True
                 (\(_ :: PermutationEdge MoveSequenceProperty [(Int,Int)]) -> True)
                 baseGen

