{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe (ticTacToeGenTests,ticTacToeGenSelfTests) where
import Spec.TicTacToe.Board
import Proper.LogicalModel
import Proper.HasLogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
--import Proper.HasPermutationGenerator.Contract
import Proper.HasPermutationGenerator.Gen
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Data.Proxy (Proxy(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad (join)

data TicTacToeMove =
  TicTacToeMove {
    from :: [Integer]
  , to   :: [Integer]
  , player :: Integer
  , declare :: Bool
  } deriving stock (Show)

-- It is tempting (and legal) to write this but it will take an age to run.
--
--data TicTacToeProperty =
--      FromBoardProperty BoardProperty
--    | ToBoardProperty BoardProperty
--    | ...
--
-- Here's why.
--
-- Model complexity!
--
-- Lifting the logic from board which has 92 possible combinations of properties
-- results in a huge model with 8464 possible combinations before we start adding
-- the game play logic which would only multiply that number further.
--
-- We can still reuse the model for board when constructing our properties and
-- generators however which will give us more elegant property definitions as well as
-- powerful control over the distributional properties of the board generator.
--
-- Using genSatisfying will give us uniform higher order randomness over the distributions
-- for each set of satisfying model properties which means edge case scenarios are less
-- likely to go unnoticed as they are prone to do with hand rolled generators.

data TicTacToeProperty =
      PlayerIsX
    | PlayerIsO
    | IsPlayersTurn
    | WinDeclared
    | IsValidWinDeclaration
    | IsValidMove
    deriving stock (Eq,Ord,Show)

$(gen_enumerable ''TicTacToeProperty)

instance LogicalModel TicTacToeProperty where
  logic = All [
--              , Var PlayerIsX :->: (Not $ Var PlayerIsO)
--              , Var PlayerIsO :->: (Not $ Var PlayerIsX)
--
--              -- X can only play when the number of pieces are equal
--              -- O can only play when there is one more X than O
--              , Var IsPlayersTurn :<->:
--                   ((Var PlayerIsX :&&: (Var $ FromBoardProperty BoardHasEqualNumberOfXsAndOs))
--                 :||: ((Var PlayerIsO) :&&: (Var $ FromBoardProperty BoardHasOneMoreXThanO)))
--              -- a win declaration is not valid if it is not the player's turn
--              , ((Not $ Var IsPlayersTurn) :&&: (Var WinDeclared))
--                       :->: (Not $ Var IsValidWinDeclaration)
--
--              -- a win decleration is not valid if the game is already won
--              , (Some [Var $ FromBoardProperty BoardContainsWinForX
--                      ,Var $ FromBoardProperty BoardContainsWinForO]
--                  :&&: Var WinDeclared) :->: (Not $ Var IsValidWinDeclaration)
--
--              -- if player X places a winning move they must declare it
--              , All (Var <$> [PlayerIsX
--                             ,ToBoardProperty BoardContainsWinForX
--                             ,IsPlayersTurn
--                             ]) :->: (Var WinDeclared :<->: Var IsValidWinDeclaration)
--
--              -- if player O places a winning move they must declare it
--              , All ([Var PlayerIsO
--                     ,Var $ ToBoardProperty BoardContainsWinForO
--                     ,Var IsPlayersTurn
--                     ]) :->: (Var WinDeclared :<->: Var IsValidWinDeclaration)
--
--              -- Player X must place an X
--              , Var PlayerIsX :->:
--                  (All [Var IsPlayersTurn
--                       ,Var $ FromBoardProperty BoardHasEqualNumberOfXsAndOs
--                       ,Var $ ToBoardProperty BoardHasOneMoreXThanO
--                       ] :<->: Var IsValidMove)
--              -- Player O must place an O
--              , (Not $ Var PlayerIsX) :->:
--                  (All [Var IsPlayersTurn
--                       ,Var $ FromBoardProperty BoardHasOneMoreXThanO
--                       ,Var $ ToBoardProperty BoardHasEqualNumberOfXsAndOs
--                       ] :<->: Var IsValidMove)
              ]

instance HasLogicalModel TicTacToeProperty TicTacToeMove where
--  satisfiesProperty (FromBoardProperty p) m = satisfiesProperty p (from m)
--  satisfiesProperty (ToBoardProperty p) m = satisfiesProperty p (from m)
  satisfiesProperty PlayerIsX m = 1 == player m
  satisfiesProperty PlayerIsO m = 2 == player m
  satisfiesProperty IsPlayersTurn m =
    case player m of
      1 -> satisfiesProperty BoardHasEqualNumberOfXsAndOs (from m)
      2 -> satisfiesProperty BoardHasOneMoreXThanO (from m)
      _ -> False
  satisfiesProperty WinDeclared m = declare m
  satisfiesProperty _ _ = False --TODO

instance HasPermutationGenerator TicTacToeProperty TicTacToeMove where
  generators =
    [ PermutationEdge
      { name = ""
      , match = Yes
      , contract = pure ()
      , permuteGen = ask
      }
    ]

instance HasParameterisedGenerator TicTacToeProperty TicTacToeMove where
  parameterisedGenerator = buildGen baseGen

baseGen :: PGen TicTacToeMove
baseGen = do
    let genTile = liftGenP (fromIntegral <$> Gen.int (linear minBound maxBound))
        -- using genSatisfying like this will generate all boards allowed by the
        -- Board model - this means we will get a model error in the TicTacToe model
        -- if we do not capture all allowed boards in its model.
        genBoard = genSatisfying (Yes :: Formula BoardProperty)
        genPlayer = genTile
        genDeclare = liftGenP $ Gen.bool
    TicTacToeMove <$> genBoard
                  <*> genBoard
                  <*> genPlayer
                  <*> genDeclare

ticTacToeGenSelfTests :: TestTree
ticTacToeGenSelfTests = testGroup "TicTacToe HasPermutationGenerator permutationGeneratorSelfTest" $
  fromGroup <$> permutationGeneratorSelfTest (\(_ :: PermutationEdge TicTacToeProperty TicTacToeMove) -> True) baseGen

ticTacToeGenTests :: TestTree
ticTacToeGenTests = testGroup "Spec TicTacToe" $
    fromGroup <$> [
      runGeneratorTestsWhere (Proxy :: Proxy TicTacToeMove)
                             "TicTacToe Generator"
                             (Yes :: Formula TicTacToeProperty)
    ]

