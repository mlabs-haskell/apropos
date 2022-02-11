{-# LANGUAGE TypeFamilies #-}
module Spec.TicTacToe ( ticTacToeTests ) where
import Proper.Script
import Hedgehog (MonadGen)
import Hedgehog.Gen (element,list)
import qualified Hedgehog.Gen as HGen
import Hedgehog.Range (linear,singleton)
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

playableBoard :: Board -> Bool
playableBoard b = length (filter isNothing (unrollBoard $ trimBoard b)) > 1

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

bothHavePlayed :: Board -> Bool
bothHavePlayed b =
  let b' = unrollBoard $ trimBoard b
   in Just X `elem` b' && Just O `elem` b'


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

gameWon :: Board -> Bool
gameWon b = winFor O b || winFor X b

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

emptyIndices :: Eq a => [Maybe a] -> [Int]
emptyIndices l = fst <$> (filter (\x -> Nothing == snd x) $ zip [0..] l)

playerIndices :: Eq a => [Maybe a] -> a -> [Int]
playerIndices l a = fst <$> (filter (\x -> Just a == snd x) $ zip [0..] l)

replaceIndex :: [a] -> Int -> a -> [a]
replaceIndex l i x = case splitAt i l of
                       (a,_:b) -> a ++ x:b
                       _ -> error "replaceIndex"

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

              -- these are added since my generator is horribly slow due to filterT
              -- they could be removed with improvements probably
              , Var GameWonAlready :->: Not (Var PlacementLegal)
              , Var GameLostAlready :->: Not (Var PlacementLegal)
              , Var GameLostAlready :->: Not (Var WinAchieved)
              , Var GameWonAlready :->: Not (Var PlayerPlacesOwnPiece)
              , Var GameWonAlready :->: Not (Var WinAchieved)
              , Not (All $ Var <$> [GameWonAlready, GameLostAlready])
              ]

  expect = All $ Var <$> [ IsPlayersTurn
                         , FromIs3by3
                         , ToIs3by3
                         , SingleTileDiff
                         , FromBoardReachable
                         ]

  genModel props = runReaderT genMoveProposal' props

genMoveProposal' :: MonadGen m => ReaderT (Set.Set (Property TicTacToe)) m (Model TicTacToe)
genMoveProposal' = do
  win <- asks (Set.member WinAchieved)
  if win
     then HGen.filterT ((flip satisfiesProperty) WinAchieved) genMoveProposalForward
     else HGen.filterT (not . (flip satisfiesProperty) WinAchieved) genMoveProposalForward

genMoveProposalForward :: MonadGen m => ReaderT (Set.Set (Property TicTacToe)) m (Model TicTacToe)
genMoveProposalForward = do
  player' <- genPlayer
  currentState <- genCurrentState player'
  nextState <- genNextState currentState player'
  win <- genDeclaration nextState
  return $ MoveProposal currentState nextState player' win
    where
      genCurrentState :: MonadGen m => Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genCurrentState p = do
        isInit <- asks (Set.member GameInInitialState)
        if isInit
           then return initialBoard
           else genRandomBoard''' p

      genNextState :: MonadGen m => Board -> Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genNextState b p = genPlacement b p >>= genModifyBoardShape

      genRandomBoard''' :: MonadGen m => Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genRandomBoard''' p = HGen.filterT playableBoard (genRandomBoard'' p)


      genRandomBoard'' :: MonadGen m => Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genRandomBoard'' p = do
        isPlayersTurn <- asks (Set.member IsPlayersTurn)
        let p' = if isPlayersTurn
                    then p
                    else otherPlayer p
        HGen.filterT (\b -> playersTurn b == p') $ do
          reachable <- asks (Set.member FromBoardReachable)
          if reachable
             then HGen.filterT reachableState $ genRandomBoard' p
             else HGen.filterT (not . reachableState) $ genRandomBoard' p

      --TODO better names for board gen
      genRandomBoard' :: MonadGen m => Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genRandomBoard' p = do
        won <- asks (Set.member GameWonAlready)
        lost <- asks (Set.member GameLostAlready)
        case (won,lost) of
          (True,True) -> HGen.filterT (\b -> winFor p b && winFor (otherPlayer p) b) (genRandomBoardo p)
          (True,_) -> HGen.filterT (\b -> winFor p b && not (winFor (otherPlayer p) b)) (genRandomBoardo p)
          (_,True) -> HGen.filterT (\b -> winFor (otherPlayer p) b && not (winFor p b)) (genRandomBoardo p)
          _ -> HGen.filterT (not .gameWon) (genRandomBoardo p)

      genRandomBoardo :: MonadGen m => Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genRandomBoardo _ = do
        ssd <- asks (Set.member PlacementLegal)
        if ssd
           then genRandomBoard
           else HGen.filterT bothHavePlayed genRandomBoard


      genRandomBoard :: MonadGen m => ReaderT (Set.Set (Property TicTacToe)) m Board
      genRandomBoard = HGen.filterT (/= initialBoard) $ do
        is3By3 <- asks (Set.member FromIs3by3)
        if is3By3
           then rerollBoard <$> list (singleton 9) genRandomTile
           else rerollBoard <$> (HGen.filterT (\x -> length x /= 9) (list (linear 6 24) genRandomTile))

--      genPlayer :: MonadGen m => Board -> ReaderT (Set.Set (Property TicTacToe)) m Player
--      genPlayer b = do
--        ipt <- asks (Set.member IsPlayersTurn)
--        return $ if ipt
--                   then playersTurn b
--                   else otherPlayer $ playersTurn b

      genPlayer :: MonadGen m => ReaderT (Set.Set (Property TicTacToe)) m Player
      genPlayer = do
        isInit <- asks (Set.member GameInInitialState)
        player' <- if isInit
                     then return O
                     else element [X,O]
        isPlayersTurn <- asks (Set.member IsPlayersTurn)
        return $ if isPlayersTurn
                   then player'
                   else otherPlayer player'

      genModifyBoardShape :: MonadGen m => Board -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genModifyBoardShape b = do
        f <- asks (Set.member FromIs3by3)
        t <- asks (Set.member ToIs3by3)
        case (f,t) of
          (False,True) -> return $ padBoard $ trimBoard b
          (_,False) -> return $ rerollBoard $ unrollBoard b ++ [Nothing] --TODO
          _ -> return b

      genPlacement :: MonadGen m => Board -> Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genPlacement bo p = do
        let b = trimBoard $ padBoard bo
        ssf <- asks (Set.member SingleTileDiff)
        if ssf
           then genSingleTileDiff b p
           else do
             ppop <- asks (Set.member PlayerPlacesOwnPiece)
             pleg <- asks (Set.member PlacementLegal)
             case (ppop,pleg) of
                (True,True) -> genLegalPlacement b (otherPlayer p) >>= \b' -> genLegalPlacement b' p
                (True,False) -> genIllegalPlacement b (otherPlayer p) >>= \b' -> genLegalPlacement b' p
                (False,False) -> genIllegalPlacement b (otherPlayer p) >>= \b' -> genLegalPlacement b' (otherPlayer p)
                (False,True) -> genLegalPlacement b (otherPlayer p) >>= \b' -> genLegalPlacement b' (otherPlayer p)

      genSingleTileDiff :: MonadGen m => Board -> Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genSingleTileDiff b p = do
        ppop <- asks (Set.member PlayerPlacesOwnPiece)
        pleg <- asks (Set.member PlacementLegal)
        let p' = if ppop
                   then p
                   else otherPlayer p
        if pleg
           then genLegalPlacement b p'
           else genIllegalPlacement b p'

      genIllegalPlacement :: MonadGen m => Board -> Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genIllegalPlacement b p = do
        let b' = unrollBoard b
            e  = filter (<9) $ playerIndices b' (otherPlayer p)
        case e of
          [] -> return b
          _ -> do
            i <- element e
            return $ rerollBoard $ replaceIndex b' i (Just p)

      genLegalPlacement :: MonadGen m => Board -> Player -> ReaderT (Set.Set (Property TicTacToe)) m Board
      genLegalPlacement b p = do
        let b' = unrollBoard b
            e  = filter (<9) $ emptyIndices b'
        case e of
          [] -> return b
          _ -> do
            i <- element e
            return $ rerollBoard $ replaceIndex b' i (Just p)

genDeclaration :: MonadGen m => Board -> ReaderT (Set.Set (Property TicTacToe)) m Bool
genDeclaration _ = do
  win <- asks (Set.member WinDeclared)
  return win

genRandomTile :: MonadGen m => ReaderT (Set.Set (Property TicTacToe)) m (Maybe Player)
genRandomTile = element [Nothing, Just X, Just O]

