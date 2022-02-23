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
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Control.Monad (join)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans (lift)

data MoveSequenceProperty =
    MoveSequenceValid
  | MoveSequenceContainsWinForX
  | MoveSequenceContainsWinForO
  deriving stock (Eq,Ord,Show)

$(gen_enumerable ''MoveSequenceProperty)

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
  logic = Var MoveSequenceValid :->: (AtMostOne $ Var <$> [MoveSequenceContainsWinForX
                                                          ,MoveSequenceContainsWinForO])

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

baseGen :: PGen [(Int,Int)]
baseGen = do
  pl <- genSatisfying (Var PlayerLocationSequencePairLengthsAreEqual)
  pure $ (uncurry zip) pl

--
--
--        > [MoveSequenceContainsWinForO]
--       /
--      /
-- * []
--     \
--      \
--       > [MoveSequenceContainsWinForX]
--
--
--
--                        > [MoveSequenceValid
--                       /  ,MoveSequenceContainsWinForO]
--                      /                                \
-- * [MoveSequenceValid]                                  \-- > [MoveSequenceValid
--                      \                                 /   ,MoveSequenceContainsWinForO]
--                       \                               /
--                        > [MoveSequenceValid          /
--                          ,MoveSequenceContainsWinForX 
--

instance HasPermutationGenerator MoveSequenceProperty [(Int,Int)] where
  generators =
    [ PermutationEdge
      { name = "InvalidateNoWin"
      , match = Yes
      , contract = clear
      , permuteGen = do
        filterPA (\s -> not (isWinFor 0 s || isWinFor 1 s))
                    $ lift
                    $ (uncurry zip) <$>
                          (genSatisfying (AtMostOne [
                             PlayerLocationSequencePairPlayer <$> playerSequenceIsValid
                           , PlayerLocationSequencePairLocation <$> locationSequenceIsValid
                           ]))
      }
    , PermutationEdge
      { name = "InvalidateMakeWinForO"
      , match = None (Var <$> enumerated)
      , contract = add MoveSequenceContainsWinForO
      , permuteGen = do
          moves <- ask
          pure moves
--          if length moves < 3
--             lseq <- filterPA ((>1) . length) $ lift $ genSatisfying (not playerSequenceIsValid)

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

