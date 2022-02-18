{-# LANGUAGE TypeFamilies #-}

module Spec.TicTacToe (ticTacToeGenTests,ticTacToeGenSelfTests) where
import Proper.HasLogicalModel
import Proper.LogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
import SAT.MiniSat ( Formula (..) )
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear,singleton)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import qualified Data.Set as Set
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))

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
    | FromBoardHasEqualNumberOfPieces
    | FromBoardHasOneMoreXThanO
    | ToBoardIsCorrectSize
    | ToBoardIsEmpty
    | ToBoardEqualToFromBoard
    | PlayerIsX
    | IsPlayersTurn
    | WinDeclared
    deriving stock (Eq,Ord,Enum,Show,Bounded)

instance LogicalModel TicTacToeProperty where
  logic = All [Var FromBoardInInitialState :<->: (All $ Var <$> [FromBoardIsCorrectSize
                                                                ,FromBoardIsEmpty
                                                                ])
              ,Var FromBoardHasOddNumberOfPieces :->: Not (Var FromBoardIsEmpty)
              ,Var IsPlayersTurn :<->:
                   ((Var PlayerIsX :&&: (Not $ Var FromBoardHasOddNumberOfPieces))
                 :||: ((Not $ Var PlayerIsX) :&&: (Var FromBoardHasOddNumberOfPieces)))
              , Var FromBoardIsEmpty :->: Var FromBoardHasEqualNumberOfPieces
              , Var FromBoardHasEqualNumberOfPieces :->: (Not $ Var FromBoardHasOddNumberOfPieces)
              , Var FromBoardHasOneMoreXThanO :->: Var FromBoardHasOddNumberOfPieces
              , Var ToBoardEqualToFromBoard :->: (All [
                   Var FromBoardIsCorrectSize :<->: Var ToBoardIsCorrectSize
                 , Var FromBoardIsEmpty :<->: Var ToBoardIsEmpty
                                                      ])
              , (All $ Var <$> [FromBoardIsCorrectSize
                               ,FromBoardIsEmpty
                               ,ToBoardIsCorrectSize
                               ,ToBoardIsEmpty]) :->: Var ToBoardEqualToFromBoard
              ]

instance HasLogicalModel TicTacToeMove TicTacToeProperty where
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
  satisfiesProperty m FromBoardHasOneMoreXThanO = numXs (from m) == (numOs (from m) + 1)
  satisfiesProperty m ToBoardEqualToFromBoard = from m == to m

instance HasPermutationGenerator TicTacToeMove TicTacToeProperty where
  generators =
    [ PermutationEdge
      { name = "MakeFromBoardCorrectSizeOdd"
      , match = (Not $ Var FromBoardHasOneMoreXThanO)
           :&&: (Not $ Var FromBoardIsCorrectSize)
           :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.insert FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genBoardWithOddNumberOfPiecesWithSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardCorrectSizeHaveOneMoreXThanO"
      , match = (Var FromBoardHasOneMoreXThanO)
           :&&: (Not $ Var FromBoardIsCorrectSize)
           :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.insert FromBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- genBoardWithFromBoardHasOneMoreXThanOWithSize 9
          pure $ m { from = b }
      }
    ,PermutationEdge
      { name = "MakeFromBoardCorrectSizeEven"
      , match = (Not $ Var FromBoardIsCorrectSize)
           :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
           :&&: (Not $ Var FromBoardIsEmpty)
      , contract = Set.insert FromBoardIsCorrectSize . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          b <- Gen.filter (/= to m) $ genBoardWithEvenNumberOfPiecesWithSize 9
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
      , contract = \s ->
          if ToBoardIsCorrectSize `elem` s && ToBoardIsEmpty `elem` s
             then Set.insert FromBoardIsCorrectSize
                $ Set.insert ToBoardEqualToFromBoard
                $ Set.insert FromBoardInInitialState s
             else Set.insert FromBoardIsCorrectSize
                $ Set.insert FromBoardInInitialState s
      , permuteGen = \m -> do
          b <- pure $ genEmptyBoardOfSize 9
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeHaveOneMoreXThanO"
      , match = (Var FromBoardHasOneMoreXThanO)
           :&&: Var FromBoardIsCorrectSize
           :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.delete FromBoardIsCorrectSize . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 1 100
          b <- Gen.filter (/= to m) $ genBoardWithFromBoardHasOneMoreXThanOWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeOdd"
      , match = (Not $ Var ToBoardEqualToFromBoard)
           :&&: (Not $ Var FromBoardHasOneMoreXThanO)
           :&&: Var FromBoardIsCorrectSize
           :&&: Var FromBoardHasOddNumberOfPieces
      , contract = Set.delete FromBoardIsCorrectSize . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 1 100
          b <- Gen.filter (/= to m) $ genBoardWithOddNumberOfPiecesWithSize i
          pure $ m { from = b }

      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeEqualNumPieces"
      , match =  (Not $ Var ToBoardEqualToFromBoard)
            :&&: Var FromBoardIsCorrectSize
            :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
      , contract = Set.delete FromBoardIsCorrectSize
                 . Set.delete FromBoardInInitialState
                 . Set.delete FromBoardIsEmpty
                 . Set.insert FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 2 100
          b <- Gen.filter (not . boardIsEmpty)
             $ Gen.filter (/= to m)
             $ genBoardWithEqualNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardIncorrectSizeEven"
      , match =  (Not $ Var ToBoardEqualToFromBoard)
            :&&: Var FromBoardIsCorrectSize
            :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
      , contract = Set.delete FromBoardIsCorrectSize
                 . Set.delete FromBoardInInitialState
                 . Set.delete FromBoardIsEmpty
                 . Set.delete FromBoardHasEqualNumberOfPieces
      , permuteGen = \m -> do
          i <- genNot9 2 100
          b <- Gen.filter (not . boardIsEmpty)
             $ Gen.filter (/= to m) $ genBoardWithEvenNumberOfPiecesWithSize i
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveOneMoreXThanO"
      , match = (Not $ Var ToBoardEqualToFromBoard)
           :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
      , contract = \s ->
          if PlayerIsX `elem` s
            then Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.delete IsPlayersTurn
               $ Set.insert FromBoardHasOneMoreXThanO s
            else Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.insert IsPlayersTurn
               $ Set.insert FromBoardHasOneMoreXThanO s
      , permuteGen = \m -> do
               b <- Gen.filter (/= to m)
                  $ genBoardWithFromBoardHasOneMoreXThanOWithSize (length (from m))
               pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveOddNumberOfPieces"
      , match = (Not $ Var ToBoardEqualToFromBoard)
           :&&: (Not $ Var FromBoardHasOddNumberOfPieces)
      , contract = \s ->
          if PlayerIsX `elem` s
            then Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.delete IsPlayersTurn s
            else Set.insert FromBoardHasOddNumberOfPieces
               $ Set.delete FromBoardHasEqualNumberOfPieces
               $ Set.insert IsPlayersTurn s
      , permuteGen = \m -> do
               b <- Gen.filter (/= to m)
                  $ genBoardWithOddNumberOfPiecesWithSize (length (from m))
               pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveEqualNumberOfPieces"
      , match = (Not $ Var ToBoardEqualToFromBoard) :&&: Var FromBoardHasOddNumberOfPieces :&&: Not (Var FromBoardIsCorrectSize)
      , contract = \s ->
          if PlayerIsX `elem` s
             then Set.delete FromBoardHasOddNumberOfPieces
                $ Set.insert FromBoardHasEqualNumberOfPieces
                $ Set.insert IsPlayersTurn
                $ Set.delete FromBoardHasOneMoreXThanO s
             else Set.delete FromBoardHasOddNumberOfPieces
                $ Set.insert FromBoardHasEqualNumberOfPieces
                $ Set.delete IsPlayersTurn
                $ Set.delete FromBoardHasOneMoreXThanO s
      , permuteGen = \m -> do
          let l = length $ from m
              s = max 2 l
          b <- Gen.filter (not . boardIsEmpty)
                 $ Gen.filter (/= to m) $ genBoardWithEqualNumberOfPiecesWithSize s
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardHaveEvenNumberOfPieces"
      , match = Var FromBoardHasOddNumberOfPieces :&&: Not (Var FromBoardIsCorrectSize)
      , contract = \s ->
          if PlayerIsX `elem` s
             then Set.delete FromBoardHasOddNumberOfPieces $ Set.delete FromBoardHasOneMoreXThanO $ Set.insert IsPlayersTurn s
             else Set.delete FromBoardHasOddNumberOfPieces $ Set.delete FromBoardHasOneMoreXThanO $ Set.delete IsPlayersTurn s
      , permuteGen = \m -> do
          let l = length $ from m
          b <- Gen.filter (/= to m) $ genBoardWithEvenNumberOfPiecesWithSize l
          if to m == from m
             then pure $ m { from = b, to = b }
             else pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeToBoardCorrectSizeEmpty"
      , match = Not (Var ToBoardIsCorrectSize) :&&: Var ToBoardIsEmpty
      , contract = \s -> if FromBoardInInitialState `elem` s
                            then Set.insert ToBoardIsCorrectSize
                               $ Set.insert ToBoardEqualToFromBoard
                               $ Set.insert ToBoardIsEmpty s
                            else Set.insert ToBoardIsCorrectSize
                               $ Set.insert ToBoardIsEmpty s
      , permuteGen = \m -> do
          let b = genEmptyBoardOfSize 9
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeToBoardCorrectSize"
      , match = Not (Var ToBoardEqualToFromBoard) :&&: Not (Var ToBoardIsCorrectSize) :&&: Not (Var ToBoardIsEmpty)
      , contract = Set.insert ToBoardIsCorrectSize . Set.delete ToBoardIsEmpty
      , permuteGen = \m -> do
          b <- Gen.filter (/= from m) $ genNonEmptyBoardOfSize 9
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeEmptyToBoardIncorrectSize"
      , match = Var ToBoardIsCorrectSize :&&: Var ToBoardIsEmpty
      , contract = \s ->
          if FromBoardInInitialState `elem` s
            then Set.delete ToBoardIsCorrectSize
               $ Set.delete ToBoardEqualToFromBoard s
            else Set.delete ToBoardIsCorrectSize s
      , permuteGen = \m -> do
          b <- Gen.filter (/= from m) $ genEmptyBoardOfIncorrectSize
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeToBoardIncorrectSize"
      , match = Var ToBoardIsCorrectSize :&&: (Not (Var ToBoardIsEmpty))
      , contract = Set.delete ToBoardIsCorrectSize
      , permuteGen = \m -> do
          b <- Gen.filter (/= from m) $ genNonEmptyBoardOfIncorrectSize
          pure $ m { to = b }
      }
    , PermutationEdge
      { name = "MakeFromBoardEmpty"
      , match = (Not $ Var FromBoardIsEmpty) :&&: (Not $ Var ToBoardIsEmpty)
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
          b <- Gen.filter (/= to m) $ genBoardWithEvenNumberOfPiecesWithSize $ max 2 (length $ from m)
          pure $ m { from = b }
      }
    , PermutationEdge
      { name = "MakeToBoardEmpty"
      , match = (Not $ Var ToBoardIsEmpty) :&&: (Not $ Var FromBoardIsEmpty)
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
          b <- Gen.filter (/= from m) $ genNonEmptyBoardOfSize (length (to m))
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
    , PermutationEdge
      { name = "SetToBoardEqualToFromBoard"
      , match = Not $ Var ToBoardEqualToFromBoard
      , contract = \s ->
          case (FromBoardIsCorrectSize `elem` s, FromBoardIsEmpty `elem` s) of
            (True,True) -> Set.insert ToBoardIsCorrectSize
                         $ Set.insert ToBoardIsEmpty
                         $ Set.insert ToBoardEqualToFromBoard s
            (True,False) -> Set.insert ToBoardIsCorrectSize
                         $ Set.insert ToBoardEqualToFromBoard s
            (False,True) -> Set.insert ToBoardIsEmpty
                         $ Set.insert ToBoardEqualToFromBoard s
            (False,False) -> Set.insert ToBoardEqualToFromBoard s
      , permuteGen = \m -> pure $ m { to = from m }
      }
    , PermutationEdge
      { name = "UnsetToBoardEqualToFromBoard"
      , match = Var ToBoardEqualToFromBoard
      , contract = \s -> Set.delete ToBoardEqualToFromBoard s
      , permuteGen = \m ->
          if boardIsEmpty $ to m
             then do
               l <- Gen.filter (/= (length (from m))) $ genNot9 0 100
               pure $ m { to = genEmptyBoardOfSize l }
             else do
                nt <- Gen.filter (/= (to m)) $ Gen.shuffle (to m)
                pure $ m { to = nt }
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
  let notFromBoardHasOneMoreXThanO = Gen.filter (\b -> not ((numXs b) == ((numOs b) + 1)))
  pieces <- notFromBoardHasOneMoreXThanO $ Gen.list (singleton p) (Gen.element [Just O, Just X])
  Gen.shuffle $ pieces <> (replicate (s - p) Nothing)

genBoardWithFromBoardHasOneMoreXThanOWithSize :: Int -> Gen [Maybe Player]
genBoardWithFromBoardHasOneMoreXThanOWithSize s = do
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


ticTacToeGenSelfTests :: TestTree
ticTacToeGenSelfTests = testGroup "TicTacToe HasPermutationGenerator permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge TicTacToeMove TicTacToeProperty) -> True) baseGen

ticTacToeGenTests :: TestTree
ticTacToeGenTests = testGroup "Spec TicTacToe" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy TicTacToeMove)
                             "TicTacToe Generator"
                             (Yes :: Formula TicTacToeProperty)
    ]

