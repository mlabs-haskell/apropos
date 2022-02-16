{-# LANGUAGE TypeFamilies #-}

module Spec.TicTacToe (ticTacToeGenTests) where
import Proper.HasProperties
import Proper.Proposition
import Proper.HasParameterisedGenerator
import Proper.PermutingGenerator
import SAT.MiniSat ( Formula (..) )
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear,singleton)
import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import qualified Data.Set as Set
import Data.Maybe (isNothing)

data Player = X | O deriving stock (Show)

data TicTacToeMove =
  TicTacToeMove {
    from :: [Maybe Player]
  , to   :: [Maybe Player]
  , player :: Player
  , declare :: Bool
  } deriving stock (Show)

data TicTacToeProperty =
      FromBoardIsCorrectSize
    | ToBoardIsCorrectSize
    | FromBoardIsEmpty
    | ToBoardIsEmpty
    | WinDeclared
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Proposition TicTacToeProperty where
  logic = Yes

instance HasProperties TicTacToeMove TicTacToeProperty where
  satisfiesProperty m FromBoardIsCorrectSize = 9 == length (from m)
  satisfiesProperty m ToBoardIsCorrectSize = 9 == length (to m)
  satisfiesProperty m FromBoardIsEmpty = all isNothing (from m)
  satisfiesProperty m ToBoardIsEmpty = all isNothing (to m)
  satisfiesProperty m WinDeclared = declare m

instance PermutingGenerator TicTacToeMove TicTacToeProperty where
  generators =
    [ PermutationEdge
      { name = "MakeFromBoardCorrectSize"
      , match = Not $ Var FromBoardIsCorrectSize
      , contract = Set.insert FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- if satisfiesProperty m FromBoardIsEmpty
                 then pure $ genEmptyBoardOfSize 9
                 else genNonEmptyBoardOfSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeEmptyFromBoardIncorrectSize"
      , match = Var FromBoardIsCorrectSize :&&: Var FromBoardIsEmpty
      , contract = Set.delete FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genEmptyBoardOfIncorrectSize
          pure $ m { from = b }
      } -- it's okay to have overlapping edges
        -- they will be chosen from at random
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSize"
      , match = Var FromBoardIsCorrectSize
      , contract = Set.delete FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- if satisfiesProperty m FromBoardIsEmpty
                 then genEmptyBoardOfIncorrectSize
                 else genNonEmptyBoardOfIncorrectSize
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeToBoardCorrectSize"
      , match = Not (Var ToBoardIsCorrectSize) :&&: Var ToBoardIsEmpty
      , contract = Set.insert ToBoardIsCorrectSize
      , permuteGen = \m -> do
          let b = genEmptyBoardOfSize 9
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeToBoardCorrectSize"
      , match = Not (Var ToBoardIsCorrectSize) :&&: Not (Var ToBoardIsEmpty)
      , contract = Set.insert ToBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genNonEmptyBoardOfSize 9
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeEmptyToBoardIncorrectSize"
      , match = Var ToBoardIsCorrectSize :&&: Var ToBoardIsEmpty
      , contract = Set.delete ToBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genEmptyBoardOfIncorrectSize
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeToBoardIncorrectSize"
      , match = Var ToBoardIsCorrectSize :&&: (Not (Var ToBoardIsEmpty))
      , contract = Set.delete ToBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genNonEmptyBoardOfIncorrectSize
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardEmpty"
      , match = Not $ Var FromBoardIsEmpty
      , contract = Set.insert FromBoardIsEmpty
      , permuteGen = \m -> do
          let b = genEmptyBoardOfSize (length (from m))
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardNotEmpty"
      , match = Var FromBoardIsEmpty
      , contract = Set.delete FromBoardIsEmpty
      , permuteGen = \m -> do
          b <- genNonEmptyBoardOfSize (length (from m))
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeToBoardEmpty"
      , match = Not $ Var ToBoardIsEmpty
      , contract = Set.insert ToBoardIsEmpty
      , permuteGen = \m -> do
          let b = genEmptyBoardOfSize (length (to m))
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeToBoardNotEmpty"
      , match = Var ToBoardIsEmpty
      , contract = Set.delete ToBoardIsEmpty
      , permuteGen = \m -> do
          b <- genNonEmptyBoardOfSize (length (to m))
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "DeclareWin"
      , match = Not $ Var WinDeclared
      , contract = Set.insert WinDeclared
      , permuteGen = \m -> pure $ m { declare = True }
      }
    , PermutationEdge
      { name = "UndeclareWin"
      , match = Var WinDeclared
      , contract = Set.delete WinDeclared
      , permuteGen = \m -> pure $ m { declare = False }
      }
    ]

boardIsEmpty :: [Maybe Player] -> Bool
boardIsEmpty = all isNothing

genTile :: Gen (Maybe Player)
genTile = Gen.element [Nothing, Just X, Just O]

genEmptyBoardOfSize :: Int -> [Maybe Player]
genEmptyBoardOfSize size = replicate size Nothing

genNonEmptyBoardOfSize :: Int -> Gen [Maybe Player]
genNonEmptyBoardOfSize size =
  Gen.filter (not . boardIsEmpty) $ Gen.list (singleton size) genTile

genNonEmptyBoardOfIncorrectSize :: Gen [Maybe Player]
genNonEmptyBoardOfIncorrectSize = Gen.filter (not . boardIsEmpty) $ do
   b <- Gen.bool
   if b
      then Gen.list (linear 0 8) genTile
      else Gen.list (linear 10 100) genTile

genEmptyBoardOfIncorrectSize :: Gen [Maybe Player]
genEmptyBoardOfIncorrectSize = do
   b <- Gen.bool
   if b
      then Gen.list (linear 0 8) $ pure Nothing
      else Gen.list (linear 10 100) $ pure Nothing

instance HasParameterisedGenerator TicTacToeMove TicTacToeProperty where
  parameterisedGenerator = buildGen $ do
    let genBoard = Gen.list (linear 0 100) genTile
        genPlayer = Gen.element [X,O]
        genDeclare = Gen.bool
    TicTacToeMove <$> genBoard
                  <*> genBoard
                  <*> genPlayer
                  <*> genDeclare

ticTacToeGenTests :: TestTree
ticTacToeGenTests = testGroup "Spec TicTacToe" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy TicTacToeMove)
                             "TicTacToe Generator"
                             (Yes :: Formula TicTacToeProperty)
    ]

