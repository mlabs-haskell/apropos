{-# LANGUAGE TypeFamilies #-}
module Spec.TicTacToe ( ticTacToeTests ) where
import Proper.Script
--import Hedgehog (MonadGen)
import Hedgehog.Gen (element,list)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (singleton, linear)
import Control.Monad.Reader
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Data.Maybe (isNothing)
import Data.List (transpose)

ticTacToeTests :: TestTree
ticTacToeTests =
  testGroup
    "TicTacToe"
    $ fromGroup <$>
      [ testEnumeratedScenarios Model "Model Consistency" modelTestGivenProperties Yes
      ]


-- a silly game
data Player = X | O deriving stock (Show,Eq)

data Board = Board [[Maybe Player]] deriving stock (Show,Eq)

initialBoard :: Board
initialBoard = Board $ replicate 3 (replicate 3 Nothing)

numNoughts :: Board -> Int
numNoughts = length . filter (== Just O) . unrollBoard

numCrosses :: Board -> Int
numCrosses = length . filter (== Just X) . unrollBoard

numPieces :: Board -> Int
numPieces b = numNoughts b + numCrosses b

reachableState :: Board -> Bool
reachableState b = let nn = numNoughts b
                       nc = numCrosses b
                    in (nc == nn + 1 || nc == nn) && boardIs3By3 b

boardIs3By3 :: Board -> Bool
boardIs3By3 (Board b) = (all (==3) (length <$> b)) && (3 == length b)

--playableBoard :: Board -> Bool
--playableBoard b = length (filter isNothing (unrollBoard $ trimBoard b)) > 1

placementLegal :: Board -> Board -> Bool
placementLegal a b =
  let at@(Board af) = trimBoard a
      bt@(Board bf) = trimBoard b
   in (all id $ (zipWith placementLegal' af bf)) && bt `hasMorePiecesThan` at
  where
    placementLegal' a' b' = all id $ zipWith (\a'' b'' -> case a'' of
                                                            Just so -> b'' == Just so
                                                            _ -> True) a' b'

hasMorePiecesThan :: Board -> Board -> Bool
hasMorePiecesThan b a = numPieces b > numPieces a

--bothHavePlayed :: Board -> Bool
--bothHavePlayed b =
--  let b' = unrollBoard $ trimBoard b
--   in Just X `elem` b' && Just O `elem` b'


-- (screaming intensifies)
(!!!) :: [Maybe a] -> Int -> Maybe a
(!!!) l i = if i < length l
               then l!!i
               else Nothing

(!!!!) :: [[a]] -> Int -> [a]
(!!!!) l i = if i < length l
                then l!!i
                else []

winFor :: Player -> Board -> Bool
winFor p board =
  let Board b = padBoard $ trimBoard board
   in any rowWin b || any rowWin (transpose b)
                   || rowWin [(b!!!!0)!!!0,(b!!!!1)!!!1,(b!!!!2)!!!2]
                   || rowWin [(b!!!!0)!!!2,(b!!!!1)!!!1,(b!!!!2)!!!0]
  where rowWin = all (== Just p)

--gameWon :: Board -> Bool
--gameWon b = winFor O b || winFor X b

unrollBoard :: Board -> [Maybe Player]
unrollBoard (Board b) = join b

rerollBoard :: [Maybe Player] -> Board
rerollBoard l = Board [take 3 l, take 3 $ drop 3 l, drop 6 l]

trimBoard :: Board -> Board
trimBoard b = rerollBoard $ take 9 $ unrollBoard b

padBoard :: Board -> Board
padBoard b =
  let b' = unrollBoard b
      l = length b'
  in rerollBoard $ b' ++ (replicate (9-l) Nothing)

diffIndices :: Board -> Board -> [Int]
diffIndices a b =
  let eqs = zipWith (==) (unrollBoard $ padBoard $ trimBoard a) (unrollBoard $ padBoard $ trimBoard b)
   in fst <$> (filter (\x -> not (snd x)) $ zip [0..] eqs)

playerPiecePlaced :: Board -> Board -> Player -> Bool
playerPiecePlaced a b p = any id $ zipWith (\a' b' -> (a' /= Just p) && (b' == Just p)) (unrollBoard $ padBoard a) (unrollBoard $ padBoard b)

--emptyIndices :: Eq a => [Maybe a] -> [Int]
--emptyIndices l = fst <$> (filter (\x -> Nothing == snd x) $ zip [0..] l)
--
--playerIndices :: Eq a => [Maybe a] -> a -> [Int]
--playerIndices l a = fst <$> (filter (\x -> Just a == snd x) $ zip [0..] l)
--
--replaceIndex :: [a] -> Int -> a -> [a]
--replaceIndex l i x = case splitAt i l of
--                       (a,_:b) -> a ++ x:b
--                       _ -> error "replaceIndex"

playersTurn :: Board -> Player
playersTurn (Board b) = let m = length $ filter isNothing $ join b
                          in if (m `mod` 2) == 0
                                then X
                                else O

otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X


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
        GameInInitialState
      | FromIs3by3
      | ToIs3by3
      | FromBoardReachable

      | IsPlayersTurn
      | PlayerPlacesOwnPiece
      | SingleTileDiff
      | PlacementLegal

      | GameWonAlready
      | GameLostAlready
      | WinAchieved
      | WinDeclared
    deriving stock (Bounded, Eq, Enum, Ord, Show)

  satisfiesProperty (MoveProposal f _ _ _) GameInInitialState = f == initialBoard
  satisfiesProperty (MoveProposal f _ p _) IsPlayersTurn = playersTurn f == p
  satisfiesProperty (MoveProposal f _ _ _) FromIs3by3 = boardIs3By3 f
  satisfiesProperty (MoveProposal _ t _ _) ToIs3by3   = boardIs3By3 t
  satisfiesProperty (MoveProposal f t _ _) SingleTileDiff = 1 == length (diffIndices f t)
  satisfiesProperty (MoveProposal f _ _ _) FromBoardReachable = reachableState f
  satisfiesProperty (MoveProposal _ _ _ w) WinDeclared = w
  satisfiesProperty (MoveProposal f _ p _) GameWonAlready = winFor p f
  satisfiesProperty (MoveProposal f _ p _) GameLostAlready = winFor (otherPlayer p) f
  satisfiesProperty (MoveProposal _ t p _) WinAchieved = winFor p t
  satisfiesProperty (MoveProposal f t p _) PlayerPlacesOwnPiece = playerPiecePlaced f t p
  satisfiesProperty (MoveProposal f t _ _) PlacementLegal = placementLegal f t

  logic = All [ Var GameInInitialState :->: (All $ Var <$> [FromIs3by3,FromBoardReachable,PlacementLegal])
              , Var GameInInitialState :->: (All $ Not . Var <$> [GameWonAlready,WinAchieved])
              , Var WinAchieved :->: Not (Var GameInInitialState)
              , (Var WinAchieved :&&: Not (Var GameWonAlready)) :->: Var PlayerPlacesOwnPiece
              , Var FromBoardReachable :->: Var FromIs3by3

              , Not (All $ Var <$> [GameWonAlready, GameLostAlready])
              ]

  expect = All $ Var <$> [ IsPlayersTurn
                         , FromIs3by3
                         , ToIs3by3
                         , SingleTileDiff
                         , FromBoardReachable
                         ]

  genBaseModel = do
    player' <- element [X,O]
    currentState <- rerollBoard <$> list (linear 6 24) (element [Nothing,Just X,Just O])
    nextState <- rerollBoard <$> list (linear 6 24) (element [Nothing,Just X,Just O])
    win <- Gen.bool
    return $ MoveProposal currentState nextState player' win

  data Transformation TicTacToe =
      SetWinDeclared
    | UnSetWinDeclared
    | SetToIs3By3
    | SetFromIs3By3
    | SetGameInInitialState
    deriving stock (Bounded, Eq, Enum, Ord, Show)

  modelTransformation SetWinDeclared m = return $ m { declare = True }
  modelTransformation UnSetWinDeclared m = return $ m { declare = False }
  modelTransformation SetToIs3By3 m = do
    t33 <- rerollBoard <$> list (singleton 9) (element [Nothing,Just X,Just O])
    return $ m { to = t33 }
  modelTransformation SetFromIs3By3 m = do
    f33 <- rerollBoard <$> list (singleton 9) (element [Nothing,Just X,Just O])
    return $ m { from = f33 }
  modelTransformation SetGameInInitialState m = pure $ m { from = initialBoard }


  propertyTransformation SetWinDeclared = (Yes, Set.insert WinDeclared)
  propertyTransformation UnSetWinDeclared = (Yes, Set.delete WinDeclared)
  propertyTransformation SetToIs3By3 = (Not (Var ToIs3by3), Set.insert ToIs3by3)
  propertyTransformation SetFromIs3By3 = (Not (Var FromIs3by3), Set.insert FromIs3by3)
  propertyTransformation SetGameInInitialState = (Not (Var GameInInitialState), Set.insert GameInInitialState)


