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

data Player = X | O deriving stock (Eq,Show)

data TicTacToeMove =
  TicTacToeMove {
    from :: [Maybe Player]
  , to   :: [Maybe Player]
  , player :: Player
  , declare :: Bool
  } deriving stock (Show)

data TicTacToeProperty =
      FromBoardIsCorrectSize
    | FromBoardIsEmpty
    | FromBoardInInitialState
    | ToBoardIsCorrectSize
    | ToBoardIsEmpty
    | PlayerIsX
    | OddNumberOfPiecesOnBoard
    | EqualNumbersOfPiecesOnBoard
    | IsPlayersTurn
    | WinDeclared
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Proposition TicTacToeProperty where
  logic = All [Var FromBoardInInitialState :<->: (All $ Var <$> [FromBoardIsCorrectSize
                                                                ,FromBoardIsEmpty
                                                                ])
              ,Var OddNumberOfPiecesOnBoard :->: Not (Var FromBoardIsEmpty)
              ,Var IsPlayersTurn :<->:
                   ((Var PlayerIsX :&&: (Not $ Var OddNumberOfPiecesOnBoard))
                 :||: ((Not $ Var PlayerIsX) :&&: (Var OddNumberOfPiecesOnBoard)))
              , Var FromBoardIsEmpty :->: Var EqualNumbersOfPiecesOnBoard
              , Var EqualNumbersOfPiecesOnBoard :->: (Not $ Var OddNumberOfPiecesOnBoard)
              ]

instance HasProperties TicTacToeMove TicTacToeProperty where
  satisfiesProperty m FromBoardIsCorrectSize = 9 == length (from m)
  satisfiesProperty m FromBoardIsEmpty = all isNothing (from m)
  satisfiesProperty m FromBoardInInitialState = 9 == length (from m) &&  all isNothing (from m)
  satisfiesProperty m ToBoardIsCorrectSize = 9 == length (to m)
  satisfiesProperty m ToBoardIsEmpty = all isNothing (to m)
  satisfiesProperty m WinDeclared = declare m
  satisfiesProperty m PlayerIsX = X == player m
  satisfiesProperty m OddNumberOfPiecesOnBoard = oddNumberOfPiecesOnBoard $ from m
  satisfiesProperty m IsPlayersTurn =
    (oddNumberOfPiecesOnBoard (from m) && O == (player m))
    || ((not (oddNumberOfPiecesOnBoard (from m))) && X == (player m))
  satisfiesProperty m EqualNumbersOfPiecesOnBoard = numXs (from m) == numOs (from m)

instance PermutingGenerator TicTacToeMove TicTacToeProperty where
  generators =
    [ PermutationEdge
      { name = "MakeFromBoardCorrectSizeOdd"
      , match = (Not $ Var FromBoardIsCorrectSize) :&&: Var OddNumberOfPiecesOnBoard
      , contract = Set.insert FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genBoardWithOddNumberOfPiecesWithSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardCorrectSizeEven"
      , match = (Not $ Var FromBoardIsCorrectSize)
           :&&: (Not $ Var OddNumberOfPiecesOnBoard)
           :&&: (Not $ Var FromBoardIsEmpty)
      , contract = Set.insert FromBoardIsCorrectSize . Set.delete EqualNumbersOfPiecesOnBoard
      , permuteGen = \m -> do
          b <- Gen.filter (not . boardIsEmpty) $ genBoardWithEvenNumberOfPiecesWithSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardCorrectSizeEqual"
      , match = (Not $ Var FromBoardIsCorrectSize)
           :&&: (Not $ Var OddNumberOfPiecesOnBoard)
           :&&: (Not $ Var FromBoardIsEmpty)
      , contract = Set.insert FromBoardIsCorrectSize . Set.insert EqualNumbersOfPiecesOnBoard
      , permuteGen = \m -> do
          b <- Gen.filter (not . boardIsEmpty) $ genBoardWithEqualNumberOfPiecesWithSize 9
          pure $ m { from = b }
      }
    ,PermutationEdge
      { name = "MakeFromBoardCorrectSizeEmpty"
      , match = (Not $ Var FromBoardIsCorrectSize) :&&: Var FromBoardIsEmpty
      , contract = Set.insert FromBoardIsCorrectSize . Set.insert FromBoardInInitialState
      , permuteGen = \m -> do
          b <- pure $ genEmptyBoardOfSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeEmptyFromBoardIncorrectSize"
      , match = Var FromBoardIsCorrectSize :&&: Var FromBoardIsEmpty
      , contract = Set.delete FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genEmptyBoardOfIncorrectSize
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeOdd"
      , match = Var FromBoardIsCorrectSize :&&: Var OddNumberOfPiecesOnBoard
      , contract = Set.delete FromBoardIsCorrectSize . Set.delete EqualNumbersOfPiecesOnBoard
      , permuteGen = \m -> do
          i <- Gen.filter (/= 9) (Gen.int (linear 1 100))
          b <- genBoardWithOddNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeEqualNumPieces"
      , match = Var FromBoardIsCorrectSize
          :&&: (Not $ Var OddNumberOfPiecesOnBoard)
      , contract = Set.delete FromBoardIsCorrectSize
                 . Set.delete FromBoardInInitialState
                 . Set.delete FromBoardIsEmpty
                 . Set.insert EqualNumbersOfPiecesOnBoard
      , permuteGen = \m -> do
          i <- Gen.filter (/= 9) (Gen.int (linear 2 100))
          b <- Gen.filter (not . boardIsEmpty) $ genBoardWithEqualNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeEven"
      , match = Var FromBoardIsCorrectSize
          :&&: (Not $ Var OddNumberOfPiecesOnBoard)
      , contract = Set.delete FromBoardIsCorrectSize
                 . Set.delete FromBoardInInitialState
                 . Set.delete FromBoardIsEmpty
                 . Set.delete EqualNumbersOfPiecesOnBoard
      , permuteGen = \m -> do
          i <- Gen.filter (/= 9) (Gen.int (linear 2 100))
          b <- Gen.filter (not . boardIsEmpty) $ genBoardWithEvenNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveOddNumberOfPieces"
      , match = Not $ Var OddNumberOfPiecesOnBoard
      , contract = \s ->
          if PlayerIsX `elem` s
            then Set.insert OddNumberOfPiecesOnBoard
               $ Set.delete EqualNumbersOfPiecesOnBoard
               $ Set.delete IsPlayersTurn s
            else Set.insert OddNumberOfPiecesOnBoard
               $ Set.delete EqualNumbersOfPiecesOnBoard
               $ Set.insert IsPlayersTurn s
      , permuteGen = \m -> do
          b <- genBoardWithOddNumberOfPiecesWithSize (length (from m))
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveEqualNumberOfPieces"
      , match = Var OddNumberOfPiecesOnBoard :&&: Not (Var FromBoardIsCorrectSize)
      , contract = \s ->
          if PlayerIsX `elem` s
             then Set.delete OddNumberOfPiecesOnBoard
                $ Set.insert EqualNumbersOfPiecesOnBoard
                $ Set.insert IsPlayersTurn s
             else Set.delete OddNumberOfPiecesOnBoard
                $ Set.insert EqualNumbersOfPiecesOnBoard
                $ Set.delete IsPlayersTurn s
      , permuteGen = \m -> do
          b <- Gen.filter (not . boardIsEmpty)
                 $ genBoardWithEqualNumberOfPiecesWithSize (length $ from m)
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveEvenNumberOfPieces"
      , match = Var OddNumberOfPiecesOnBoard :&&: Not (Var FromBoardIsCorrectSize)
      , contract = \s ->
          if PlayerIsX `elem` s
             then Set.delete OddNumberOfPiecesOnBoard $ Set.insert IsPlayersTurn s
             else Set.delete OddNumberOfPiecesOnBoard $ Set.delete IsPlayersTurn s
      , permuteGen = \m -> do
          b <- Gen.filter (not . boardIsEmpty)
                 $ genBoardWithEvenNumberOfPiecesWithSize (length $ from m)
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
      { name = "MakeFromBoardNotEmptyOdd"
      , match = Var FromBoardIsEmpty :&&: Var OddNumberOfPiecesOnBoard
      , contract = Set.delete FromBoardIsEmpty
      , permuteGen = \m -> do
          b <- genBoardWithOddNumberOfPiecesWithSize (length (from m))
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardNotEmptyEven"
      , match = Var FromBoardIsEmpty :&&: Not (Var OddNumberOfPiecesOnBoard)
      , contract = Set.delete FromBoardIsEmpty . Set.delete EqualNumbersOfPiecesOnBoard
      , permuteGen = \m -> do
          b <- Gen.filter (not . boardIsEmpty)
                $ genBoardWithEvenNumberOfPiecesWithSize (length (from m))
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
    , PermutationEdge
      { name = "SetPlayerIsX"
      , match = Not $ Var PlayerIsX
      , contract = \s -> if OddNumberOfPiecesOnBoard `elem` s
                           then Set.insert PlayerIsX $ Set.delete IsPlayersTurn s
                           else Set.insert PlayerIsX $ Set.insert IsPlayersTurn s
      , permuteGen = \m -> pure $ m { player = X }
      }
    , PermutationEdge
      { name = "UnsetPlayerIsX"
      , match = Var PlayerIsX
      , contract = \s -> if OddNumberOfPiecesOnBoard `elem` s
                           then Set.delete PlayerIsX $ Set.insert IsPlayersTurn s
                           else Set.delete PlayerIsX $ Set.delete IsPlayersTurn s
      , permuteGen = \m -> pure $ m { player = O }
      }
    ]

numXs :: [Maybe Player] -> Int
numXs = length . filter (== Just X)

numOs :: [Maybe Player] -> Int
numOs = length . filter (== Just O)

oddNumberOfPiecesOnBoard :: [Maybe Player] -> Bool
oddNumberOfPiecesOnBoard board = odd $ length $ filter (not . isNothing) board

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

genBoardWithOddNumberOfPiecesWithSize :: Int -> Gen [Maybe Player]
genBoardWithOddNumberOfPiecesWithSize s = do
  p <- Gen.filter odd $ Gen.int (linear 0 s)
  pieces <- Gen.list (singleton p) (Gen.element [Just O, Just X])
  Gen.shuffle $ pieces <> (replicate (s - p) Nothing)

genBoardWithEqualNumberOfPiecesWithSize :: Int -> Gen [Maybe Player]
genBoardWithEqualNumberOfPiecesWithSize s = do
  p <- Gen.filter even $ Gen.int (linear 0 s)
  let xs = replicate (p `div` 2) (Just X)
      os = replicate (p `div` 2) (Just O)
      es = (replicate (s - p) Nothing)
  Gen.shuffle $ xs <> os <> es

genBoardWithEvenNumberOfPiecesWithSize :: Int -> Gen [Maybe Player]
genBoardWithEvenNumberOfPiecesWithSize s = do
  p <- Gen.filter even $ Gen.int (linear 2 s)
  b <- Gen.int (linear 0 ((p `div` 2) - 1))
  q <- Gen.shuffle [X,O]
  let as = replicate b (Just (q!!0))
      bs = replicate (p - b) (Just (q!!1))
      es = (replicate (s - p) Nothing)
  Gen.shuffle $ as <> bs <> es

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

