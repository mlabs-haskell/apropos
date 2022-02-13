{-# LANGUAGE TypeFamilies #-}
module Spec.TicTacToe ( ticTacToeTests ) where
import Proper.Script
--import Hedgehog (MonadGen)
import Hedgehog.Gen (element,list)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (singleton,linear)
--import Control.Monad.Reader
--import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Data.Maybe (isNothing)
--import Data.List (transpose)

ticTacToeTests :: TestTree
ticTacToeTests =
  testGroup
    "TicTacToe"
    $ fromGroup <$>
      [ testEnumeratedScenarios Model "Model Consistency" modelTestGivenProperties Yes
      ]

data Player = X | O deriving stock (Show,Eq)

data Board = Board { rows :: Int
                   , cols :: Int
                   , wins :: Int
                   , board :: [Maybe Player]
                   } deriving stock (Show,Eq)

data TicTacToe = Model deriving stock (Show)

instance Proper TicTacToe where

  data Model TicTacToe =
    MoveProposal {
        from :: Board
      , to :: Board
      , player :: Player
      , declare :: Bool
      }
    deriving stock (Show)

  data Property TicTacToe =
        FromEmptyBoard
      | ToEmptyBoard
      | FromHasTooManyCells
      | FromHasTooFewCells
      | WinDeclared
    deriving stock (Bounded, Eq, Enum, Ord, Show)

  satisfiesProperty (MoveProposal f _ _ _) FromEmptyBoard = all isNothing (board f)
  satisfiesProperty (MoveProposal _ t _ _) ToEmptyBoard = all isNothing (board t)
  satisfiesProperty (MoveProposal f _ _ _) FromHasTooManyCells = length (board f) > (rows f * cols f)
  satisfiesProperty (MoveProposal f _ _ _) FromHasTooFewCells = length (board f) < (rows f * cols f)
  satisfiesProperty (MoveProposal _ _ _ w) WinDeclared = w

  logic = AtMostOne $ Var <$> [FromHasTooFewCells,FromHasTooManyCells]

  expect = Yes

  genBaseModel = do
    player' <- element [X,O]
    currentState <- list (linear 6 12) (element [Nothing])
    nextState <- list (linear 6 24) (element [Nothing,Just X,Just O])
    win <- Gen.bool
    return $ MoveProposal (Board 3 3 3 currentState) (Board 3 3 3 nextState) player' win

  transformation (On FromEmptyBoard) m@(MoveProposal f _ _ _) = return $ m { from = f { board = replicate (length (board f)) Nothing } }
  transformation (Off FromEmptyBoard) m@(MoveProposal f _ _ _) = do
    let tiles = length $ board f
        els = [Nothing,Just X,Just O]
    b <- Gen.filterT (not . all isNothing) $ list (singleton tiles) (element els)
    pure $ m { from = f { board = b } }

  transformation (On ToEmptyBoard) m@(MoveProposal _ t _ _) = return $ m { to = t { board = replicate (length (board t)) Nothing } }
  transformation (Off ToEmptyBoard) m = do
    b <- list (linear 6 12) (element [Nothing,Just X, Just O])
    pure $ m { to = Board 3 3 3 b }

  transformation (On FromHasTooManyCells) m@(MoveProposal f _ _ _) = do
    let blen = length $ board f
        tiles = rows f * cols f
        lb = max 1 ((tiles - blen) + 1)
        els = if satisfiesProperty m FromEmptyBoard
                 then [Nothing]
                 else [Nothing,Just X, Just O]
    b <- list (linear lb (lb + 4)) (element els)
    pure $ m { from = f { board = (board f) <> b } }
  transformation (Off FromHasTooManyCells) m@(MoveProposal f _ _ _) = do
    let tiles = rows f * cols f
    pure $ m { from = f { board = take tiles (board f) } }

  transformation (On FromHasTooFewCells) m@(MoveProposal f _ _ _) = do
    let blen = length $ board f
        tiles = rows f * cols f
        lb = max 1 ((blen - tiles) + 1)
    d <- Gen.int (linear lb blen)
    let nb = drop d (board f)
    if satisfiesProperty m FromEmptyBoard || not (all isNothing nb)
       then pure $ m { from = f { board = nb  } }
       else do
         let els = [Nothing, Just X, Just O]
         b <- Gen.filterT (not . all isNothing) $ list (singleton (length nb)) (element els)
         pure $ m { from = f { board = b  } }
  transformation (Off FromHasTooFewCells) m@(MoveProposal f _ _ _) = do
    let blen = length $ board f
        tiles = rows f * cols f
        lb = tiles - blen
        els = if satisfiesProperty m FromEmptyBoard
                 then [Nothing]
                 else [Nothing,Just X, Just O]
    b <- list (singleton lb) (element els)
    pure $ m { from = f { board = (board f) <> b} }


  transformation (On WinDeclared) m = return $ m { declare = True }
  transformation (Off WinDeclared) m = return $ m { declare = False }

  transformationPossible (On FromHasTooManyCells) = Not $ Var FromHasTooFewCells
  transformationPossible (On FromHasTooFewCells) = Not $ Var FromHasTooManyCells
  transformationPossible _ = Yes

