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
    | FromBoardHasOddNumberOfPieces
--    | FromBoardHasEqualNumberOfPieces -- moving this to here causes a behaviour change
--                                         we get "gave up after 100 discards" errors
--                                         suspect this is due to the path finding algorithm
--                                         it returns a different path depending on the
--                                         numbering of the nodes and this causes generators
--                                         to compose differently
--
--                                         the best solution to this may be to randomly
--                                         choose between available paths which would
--                                         flag up these filter errors as they are introduced
--                                         since this is really a hidden model failure
--                                         being revealed by changing paths
--
--                                         instead of using dfs to find a path we can
--                                         construct a distance matrix up front and
--                                         randomly choose a next node from the set of
--                                         reachable nodes that reduce the distance to
--                                         the target
    | ToBoardIsCorrectSize
    | ToBoardIsEmpty
    | PlayerIsX
    | FromBoardHasEqualNumberOfPieces
    | OneMoreXThanO
    | IsPlayersTurn
    | WinDeclared
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance Proposition TicTacToeProperty where
  logic = All [Var FromBoardInInitialState :<->: (All $ Var <$> [FromBoardIsCorrectSize
                                                                ,FromBoardIsEmpty
                                                                ])
              ,Var FromBoardHasOddNumberOfPieces :->: Not (Var FromBoardIsEmpty)
              ,Var IsPlayersTurn :<->:
                   ((Var PlayerIsX :&&: (Not $ Var FromBoardHasOddNumberOfPieces))
                 :||: ((Not $ Var PlayerIsX) :&&: (Var FromBoardHasOddNumberOfPieces)))
              , Var FromBoardIsEmpty :->: Var FromBoardHasEqualNumberOfPieces
              , Var FromBoardHasEqualNumberOfPieces :->: (Not $ Var FromBoardHasOddNumberOfPieces)
              , Var OneMoreXThanO :->: Var FromBoardHasOddNumberOfPieces
              ]

instance HasProperties TicTacToeMove TicTacToeProperty where
  satisfiesProperty m FromBoardIsCorrectSize = 9 == length (from m)
  satisfiesProperty m FromBoardIsEmpty = all isNothing (from m)
  satisfiesProperty m FromBoardInInitialState = 9 == length (from m) &&  all isNothing (from m)
  satisfiesProperty m ToBoardIsCorrectSize = 9 == length (to m)
  satisfiesProperty m ToBoardIsEmpty = all isNothing (to m)
  satisfiesProperty m WinDeclared = declare m
  satisfiesProperty m PlayerIsX = X == player m
  satisfiesProperty m FromBoardHasOddNumberOfPieces = oddNumberOfPiecesOnBoard $ from m
  satisfiesProperty m IsPlayersTurn =
    (oddNumberOfPiecesOnBoard (from m) && O == (player m))
    || ((not (oddNumberOfPiecesOnBoard (from m))) && X == (player m))
  satisfiesProperty m FromBoardHasEqualNumberOfPieces = numXs (from m) == numOs (from m)
  satisfiesProperty m OneMoreXThanO = numXs (from m) == (numOs (from m) + 1)

instance PermutingGenerator TicTacToeMove TicTacToeProperty where
  generators =
    [ PermutationEdge
      { name = "MakeFromBoardCorrectSizeOdd"
      , match = (Not $ Var OneMoreXThanO) :&&: (Not $ Var FromBoardIsCorrectSize) :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.insert FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genBoardWithOddNumberOfPiecesWithSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardCorrectSizeOneMoreXThanO"
      , match = (Var OneMoreXThanO) :&&: (Not $ Var FromBoardIsCorrectSize) :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.insert FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genBoardWithOneMoreXThanOWithSize 9
          pure $ m { from = b }
      }
    ,PermutationEdge
      { name = "MakeFromBoardCorrectSizeEven"
      , match = (Not $ Var FromBoardIsCorrectSize)
           :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
           :&&: (Not $ Var FromBoardIsEmpty)
      , contract = Set.insert FromBoardIsCorrectSize . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          b <- genBoardWithEvenNumberOfPiecesWithSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardCorrectSizeEqual"
      , match = (Not $ Var FromBoardIsCorrectSize)
           :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
           :&&: (Not $ Var FromBoardIsEmpty)
      , contract = Set.insert FromBoardIsCorrectSize . Set.insert FromBoardHasEqualNumberOfPieces
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
      { name = "MakeFromBoardIncorrectSizeOneMoreXThanO"
      , match = (Var OneMoreXThanO) :&&: Var FromBoardIsCorrectSize :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.delete FromBoardIsCorrectSize . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 1 100
          b <- genBoardWithOneMoreXThanOWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeOdd"
      , match = (Not $ Var OneMoreXThanO) :&&: Var FromBoardIsCorrectSize :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.delete FromBoardIsCorrectSize . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 1 100
          b <- genBoardWithOddNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeEqualNumPieces"
      , match = Var FromBoardIsCorrectSize
          :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
      , contract = Set.delete FromBoardIsCorrectSize
                 . Set.delete FromBoardInInitialState
                 . Set.delete FromBoardIsEmpty
                 . Set.insert FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 2 100
          b <- Gen.filter (not . boardIsEmpty) $ genBoardWithEqualNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeEven"
      , match = Var FromBoardIsCorrectSize
          :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
      , contract = Set.delete FromBoardIsCorrectSize
                 . Set.delete FromBoardInInitialState
                 . Set.delete FromBoardIsEmpty
                 . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 2 100
          b <- Gen.filter (not . boardIsEmpty) $ genBoardWithEvenNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveOneMoreXThanO"
      , match = Not $ Var FromBoardHasOddNumberOfPieces
      , contract = \s ->
          if PlayerIsX `elem` s
            then Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.delete IsPlayersTurn
               $ Set.insert OneMoreXThanO s
            else Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.insert IsPlayersTurn
               $ Set.insert OneMoreXThanO s
      , permuteGen = \m -> do
          b <- genBoardWithOneMoreXThanOWithSize (length (from m))
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveOddNumberOfPieces"
      , match = Not $ Var FromBoardHasOddNumberOfPieces
      , contract = \s ->
          if PlayerIsX `elem` s
            then Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.delete IsPlayersTurn s
            else Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.insert IsPlayersTurn s
      , permuteGen = \m -> do
          b <- genBoardWithOddNumberOfPiecesWithSize (length (from m))
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveEqualNumberOfPieces"
      , match = Var FromBoardHasOddNumberOfPieces :&&: Not (Var FromBoardIsCorrectSize)
      , contract = \s ->
          if PlayerIsX `elem` s
             then Set.delete FromBoardHasOddNumberOfPieces
                $ Set.insert FromBoardHasEqualNumberOfPieces
                $ Set.insert IsPlayersTurn
                $ Set.delete OneMoreXThanO s
             else Set.delete FromBoardHasOddNumberOfPieces
                $ Set.insert FromBoardHasEqualNumberOfPieces
                $ Set.delete IsPlayersTurn
                $ Set.delete OneMoreXThanO s
      , permuteGen = \m -> do
          let l = length $ from m
              s = max 2 l
          b <- Gen.filter (not . boardIsEmpty)
                 $ genBoardWithEqualNumberOfPiecesWithSize s
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveEvenNumberOfPieces"
      , match = Var FromBoardHasOddNumberOfPieces :&&: Not (Var FromBoardIsCorrectSize)
      , contract = \s ->
          if PlayerIsX `elem` s
             then Set.delete FromBoardHasOddNumberOfPieces $ Set.delete OneMoreXThanO $ Set.insert IsPlayersTurn s
             else Set.delete FromBoardHasOddNumberOfPieces $ Set.delete OneMoreXThanO $ Set.delete IsPlayersTurn s
      , permuteGen = \m -> do
          let l = length $ from m
          b <- genBoardWithEvenNumberOfPiecesWithSize l
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeToBoardCorrectSizeEmpty"
      , match = Not (Var ToBoardIsCorrectSize) :&&: Var ToBoardIsEmpty
      , contract = Set.insert ToBoardIsCorrectSize . Set.insert ToBoardIsEmpty
      , permuteGen = \m -> do
          let b = genEmptyBoardOfSize 9
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeToBoardCorrectSize"
      , match = Not (Var ToBoardIsCorrectSize) :&&: Not (Var ToBoardIsEmpty)
      , contract = Set.insert ToBoardIsCorrectSize . Set.delete ToBoardIsEmpty
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
      { name = "MakeFromBoardNotEmptyEven"
      , match = Var FromBoardIsEmpty :&&: Not (Var FromBoardHasOddNumberOfPieces)
      , contract = Set.delete FromBoardIsEmpty . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          b <- genBoardWithEvenNumberOfPiecesWithSize $ max 2 (length $ from m)
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
      , contract = \s -> if FromBoardHasOddNumberOfPieces `elem` s
                           then Set.insert PlayerIsX $ Set.delete IsPlayersTurn s
                           else Set.insert PlayerIsX $ Set.insert IsPlayersTurn s
      , permuteGen = \m -> pure $ m { player = X }
      }
    , PermutationEdge
      { name = "UnsetPlayerIsX"
      , match = Var PlayerIsX
      , contract = \s -> if FromBoardHasOddNumberOfPieces `elem` s
                           then Set.delete PlayerIsX $ Set.insert IsPlayersTurn s
                           else Set.delete PlayerIsX $ Set.delete IsPlayersTurn s
      , permuteGen = \m -> pure $ m { player = O }
      }
    ]

genNot9 :: Int -> Int -> Gen Int
genNot9 l u = do
  b <- Gen.bool
  if b
     then Gen.int (linear l 8)
     else Gen.int (linear 10 u)

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
  let notOneMoreXThanO = Gen.filter (\b -> not ((numXs b) == ((numOs b) + 1)))
  pieces <- notOneMoreXThanO $ Gen.list (singleton p) (Gen.element [Just O, Just X])
  Gen.shuffle $ pieces <> (replicate (s - p) Nothing)

genBoardWithOneMoreXThanOWithSize :: Int -> Gen [Maybe Player]
genBoardWithOneMoreXThanOWithSize s = do
  p <- Gen.filter even $ Gen.int (linear 0 (s-1))
  let xs = replicate ((p `div` 2) + 1) (Just X)
      os = replicate (p `div` 2) (Just O)
      es = replicate (s - (2*(p `div` 2) + 1)) Nothing
  Gen.shuffle $ xs <> os <> es

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
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen TicTacToeMove
baseGen = do
    let genBoard = Gen.list (linear 0 100) genTile
        genPlayer = Gen.element [X,O]
        genDeclare = Gen.bool
    TicTacToeMove <$> genBoard
                  <*> genBoard
                  <*> genPlayer
                  <*> genDeclare


ticTacToeGenTests :: TestTree
ticTacToeGenTests = testGroup "TicTacToe PermutingGenerator selfTest" $
  fromGroup <$> selfTest (\(_ :: PermutationEdge TicTacToeMove TicTacToeProperty) -> True) baseGen

--ticTacToeGenTests = testGroup "Spec TicTacToe" $
--    fromGroup <$> [
--      runGeneratorTestsWhere (Proxy :: Proxy TicTacToeMove)
--                             "TicTacToe Generator"
--                             (Yes :: Formula TicTacToeProperty)
--    ]

