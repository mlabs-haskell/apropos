{-# LANGUAGE TypeFamilies #-}
module Spec.TicTacToe ( ticTacToeTests ) where
import Proper.Script
import Hedgehog (MonadGen)
import Hedgehog.Gen (element,list,int)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (singleton,linear)
import Control.Monad.Reader
import qualified Data.Set as Set
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
                   , k :: Int
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
      | FromHasCorrectNumberOfCells
      | FromHasTooFewCells
      | ToHasTooManyCells
      | ToHasCorrectNumberOfCells
      | ToHasTooFewCells
      | BoardShapeChanged
 --     | BoardStateChanged
      | WinDeclared
    deriving stock (Bounded, Eq, Enum, Ord, Show)

  satisfiesProperty (MoveProposal f _ _ _) FromEmptyBoard = all isNothing (board f)
  satisfiesProperty (MoveProposal _ t _ _) ToEmptyBoard = all isNothing (board t)
  satisfiesProperty (MoveProposal f _ _ _) FromHasTooManyCells = length (board f) > (rows f * cols f)
  satisfiesProperty (MoveProposal f _ _ _) FromHasTooFewCells  = length (board f) < (rows f * cols f)
  satisfiesProperty (MoveProposal f _ _ _) FromHasCorrectNumberOfCells  = length (board f) == (rows f * cols f)
  satisfiesProperty (MoveProposal _ t _ _) ToHasTooManyCells = length (board t) > (rows t * cols t)
  satisfiesProperty (MoveProposal _ t _ _) ToHasCorrectNumberOfCells  = length (board t) == (rows t * cols t)
  satisfiesProperty (MoveProposal _ t _ _) ToHasTooFewCells  = length (board t) < (rows t * cols t)
  satisfiesProperty (MoveProposal f t _ _) BoardShapeChanged = rows t /= rows f || cols f /= cols t
  satisfiesProperty (MoveProposal _ _ _ w) WinDeclared = w
--  satisfiesProperty (MoveProposal f t _ _) BoardStateChanged = f /= t

  logic = All [ ExactlyOne $ Var <$> [FromHasTooFewCells,FromHasCorrectNumberOfCells,FromHasTooManyCells]
              , Var FromHasCorrectNumberOfCells :<->: (All $ Not . Var <$> [FromHasTooFewCells,FromHasTooManyCells])

              , ExactlyOne $ Var <$> [ToHasTooFewCells,ToHasCorrectNumberOfCells,ToHasTooManyCells]
              , Var ToHasCorrectNumberOfCells :<->: (All $ Not . Var <$> [ToHasTooFewCells,ToHasTooManyCells])

--              , Not BoardStateChanged :->: (All [ FromHasTooManyCells :<->: ToHasTooManyCells

--                                                  ])
              ]

  expect = Yes

  genBaseModel = do
    player' <- element [X,O]
    r <- int (linear 2 10)
    c <- int (linear 2 10)
    currentState <- list (linear 1 200) (element [Nothing,Just X,Just O])
    shapeChanged <- asks (Set.member BoardShapeChanged)
    (r'',c'') <- if shapeChanged
                    then Gen.filterT (/= (r,c)) $ (,) <$> int (linear 2 10) <*> int (linear 3 10)
                    else pure (r,c)
--    stateChanged <- asks (Set.member BoardStateChanged)
    stateChanged <- Gen.bool
    nextState <- if stateChanged
                    then let gl = list (linear 1 200) (element [Nothing,Just X,Just O])
                           in Gen.filterT (/= currentState) gl
                    else pure currentState
    win <- Gen.bool
    return $ MoveProposal (Board r c 3 currentState) (Board r'' c'' 3 nextState) player' win

  transformation (On FromEmptyBoard) m@(MoveProposal f _ _ _) = return $ m { from = f { board = replicate (length (board f)) Nothing } }
  transformation (Off FromEmptyBoard) m@(MoveProposal f _ _ _) = do
    let tiles = length $ board f
        els = [Nothing,Just X,Just O]
    b <- Gen.filterT (not . all isNothing) $ list (singleton tiles) (element els)
    pure $ m { from = f { board = b } }

  transformation (On ToEmptyBoard) m@(MoveProposal _ t _ _) = return $ m { to = t { board = replicate (length (board t)) Nothing } }
  transformation (Off ToEmptyBoard) m@(MoveProposal _ t _ _) = do
    let tiles = length $ board t
        els = [Nothing,Just X,Just O]
    b <- Gen.filterT (not . all isNothing) $ list (singleton tiles) (element els)
    pure $ m { to = t { board = b } }

  transformation (On FromHasTooManyCells) m@(MoveProposal f _ _ _) = do
    b <- switchOnHasTooManyCells (satisfiesProperty m FromEmptyBoard) f
    pure $ m { from = f { board = board b } }
  transformation (Off FromHasTooManyCells) m@(MoveProposal f _ _ _) = do 
    b <- switchOffHasTooManyCells (satisfiesProperty m FromEmptyBoard) f
    pure $ m { from = f { board = board b } }

  transformation (On FromHasCorrectNumberOfCells) m =
    transformation (Off FromHasTooManyCells) m >>= transformation (Off FromHasTooFewCells)
  transformation (Off FromHasCorrectNumberOfCells) m = do
    b <- Gen.bool
    if b
       then transformation (On FromHasTooManyCells) m
       else transformation (On FromHasTooFewCells) m

  transformation (On FromHasTooFewCells) m@(MoveProposal f _ _ _) = do
    b <- switchOnHasTooFewCells (satisfiesProperty m FromEmptyBoard) f
    pure $ m { from = f { board = board b } }
  transformation (Off FromHasTooFewCells) m@(MoveProposal f _ _ _) = do
    b <- switchOffHasTooFewCells (satisfiesProperty m FromEmptyBoard) f
    pure $ m { from = f { board = board b } }

  transformation (On ToHasTooManyCells) m@(MoveProposal _ t _ _) = do
    b <- switchOnHasTooManyCells (satisfiesProperty m ToEmptyBoard) t
    pure $ m { to = t { board =  board b } }
  transformation (Off ToHasTooManyCells) m@(MoveProposal _ t _ _) = do
    b <- switchOffHasTooManyCells (satisfiesProperty m ToEmptyBoard) t
    pure $ m { to = t { board = board b } }

  transformation (On ToHasCorrectNumberOfCells) m =
    transformation (Off ToHasTooManyCells) m >>= transformation (Off ToHasTooFewCells)
  transformation (Off ToHasCorrectNumberOfCells) m = do
    b <- Gen.bool
    if b
       then transformation (On ToHasTooManyCells) m
       else transformation (On ToHasTooFewCells) m

  transformation (On ToHasTooFewCells) m@(MoveProposal _ t _ _) = do
    b <- switchOnHasTooFewCells (satisfiesProperty m ToEmptyBoard) t
    pure $ m { to = t { board = board b } }
  transformation (Off ToHasTooFewCells) m@(MoveProposal _ t _ _) = do
    b <- switchOffHasTooFewCells (satisfiesProperty m ToEmptyBoard) t
    pure $ m { to = t { board = board b } }

  transformation (On WinDeclared) m = return $ m { declare = True }
  transformation (Off WinDeclared) m = return $ m { declare = False }

  transformation (On BoardShapeChanged) m = pure m --TODO?
  transformation (Off BoardShapeChanged) m@(MoveProposal f _ _ _) = pure $ m { to = f }

--  transformation (On BoardStateChanged) m = pure m --TODO?
--  transformation (Off BoardStateChanged)  m@(MoveProposal f _ _ _) = pure $ m { to = f }



  transformationImplications (Off FromHasTooManyCells) = on FromHasCorrectNumberOfCells
  transformationImplications (Off FromHasCorrectNumberOfCells) =
    ExactlyOne $ on <$> [FromHasTooFewCells,FromHasTooManyCells]
  transformationImplications (Off FromHasTooFewCells)  = on FromHasCorrectNumberOfCells
  transformationImplications (Off ToHasTooManyCells)   = on ToHasCorrectNumberOfCells
  transformationImplications (Off ToHasCorrectNumberOfCells) =
    ExactlyOne $ on <$> [ToHasTooFewCells,ToHasTooManyCells]
  transformationImplications (Off ToHasTooFewCells)    = on ToHasCorrectNumberOfCells
  transformationImplications (On FromHasTooManyCells)  = off FromHasCorrectNumberOfCells
  transformationImplications (On FromHasCorrectNumberOfCells) =
    All $ off <$> [FromHasTooFewCells,FromHasTooManyCells]
  transformationImplications (On FromHasTooFewCells)   = off FromHasCorrectNumberOfCells
  transformationImplications (On ToHasTooManyCells)    = off ToHasCorrectNumberOfCells
  transformationImplications (On ToHasCorrectNumberOfCells) =
    All $ off <$> [ToHasTooFewCells,ToHasTooManyCells]
  transformationImplications (On ToHasTooFewCells)     = off ToHasCorrectNumberOfCells

  transformationImplications _ = Yes


  transformationPossible (On FromHasTooManyCells) = Not $ Var FromHasTooFewCells
  transformationPossible (On FromHasTooFewCells) = Not $ Var FromHasTooManyCells
  transformationPossible (On ToHasTooManyCells) = Not $ Var ToHasTooFewCells
  transformationPossible (On ToHasTooFewCells) = Not $ Var ToHasTooManyCells
  transformationPossible (On BoardShapeChanged) = No
--  transformationPossible (On BoardStateChanged) = No
  transformationPossible _ = Yes


switchOnHasTooManyCells :: MonadGen m => Bool -> Board -> m Board
switchOnHasTooManyCells emptyBoard b = do
  let blen = length $ board $ b
      tiles = rows b * cols b
      lb = max 1 ((tiles - blen) + 1)
      els = if emptyBoard
               then [Nothing]
               else [Nothing,Just X, Just O]
  b' <- list (linear lb (lb + 4)) (element els)
  pure $ b { board = (board b) <> b' }

switchOffHasTooManyCells :: MonadGen m => Bool -> Board -> m Board
switchOffHasTooManyCells emptyBoard b = do
  let tiles = rows b * cols b
      els = [Nothing,Just X, Just O]
  if emptyBoard || (not $ all isNothing $ take tiles $ board b)
     then pure b { board = take tiles $ board b }
     else do
       let gl = list (singleton tiles) (element els)
       b' <- Gen.filterT (\c -> not (all isNothing c)) gl
       pure $ b { board = b' }

switchOnHasTooFewCells :: MonadGen m => Bool -> Board -> m Board
switchOnHasTooFewCells emptyBoard b = do
    let blen = length $ board b
        tiles = rows b * cols b
        lb = max 1 ((blen - tiles) + 1)
    d <- Gen.int (linear lb blen)
    let nb = drop d $ board b
    if emptyBoard || not (all isNothing nb)
       then pure $ b { board = nb }
       else do
         let els = [Nothing, Just X, Just O]
         b' <- Gen.filterT (not . all isNothing) $ list (singleton (length nb)) (element els)
         pure $ b { board = b'}

switchOffHasTooFewCells :: MonadGen m => Bool -> Board -> m Board
switchOffHasTooFewCells emptyBoard b = do
    let blen = length $ board b
        tiles = rows b * cols b
        lb = tiles - blen
        els = if emptyBoard
                 then [Nothing]
                 else [Nothing,Just X, Just O]
    b' <- list (singleton lb) (element els)
    pure $ b { board = (board b) <> b' }

