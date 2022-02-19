{-# LANGUAGE TemplateHaskell #-}

module Spec.TicTacToe (ticTacToeGenTests,ticTacToeGenSelfTests) where
--import Debug.Trace
import Spec.TicTacToe.Board
import Proper.LogicalModel
import Proper.HasLogicalModel
import Proper.HasParameterisedGenerator
import Proper.HasPermutationGenerator
--import Proper.HasPermutationGenerator.Contract
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Range (linear)
import Test.Tasty (TestTree,testGroup)
import Test.Tasty.Hedgehog (fromGroup)
import Data.Proxy (Proxy(..))
--import Control.Monad.Trans.Reader (ask)
import Control.Monad (join)

data TicTacToeMove =
  TicTacToeMove {
    from :: [Integer]
  , to   :: [Integer]
  , player :: Integer
  , declare :: Bool
  } deriving stock (Show)

data TicTacToeProperty =
      FromBoardProperty BoardProperty
    | ToBoardProperty BoardProperty
    | PlayerIsX
    | IsPlayersTurn
    | WinDeclared
    deriving stock (Eq,Ord,Show)

$(gen_enumerable ''TicTacToeProperty)

instance LogicalModel TicTacToeProperty where
  logic = All [ FromBoardProperty <$> logic
              , ToBoardProperty <$> logic
              , Var IsPlayersTurn :<->:
                   ((Var PlayerIsX :&&: (Var $ FromBoardProperty BoardHasEqualNumberOfXsAndOs))
                 :||: ((Not $ Var PlayerIsX) :&&: (Var $ FromBoardProperty BoardHasOneMoreXThanO)))
              ]

instance HasLogicalModel TicTacToeProperty TicTacToeMove where
  satisfiesProperty (FromBoardProperty p) m = satisfiesProperty p (from m)
  satisfiesProperty (ToBoardProperty p) m = satisfiesProperty p (from m)
  satisfiesProperty PlayerIsX m = 1 == player m
  satisfiesProperty IsPlayersTurn m =
    case player m of
      1 -> satisfiesProperty BoardHasEqualNumberOfXsAndOs (from m)
      2 -> satisfiesProperty BoardHasOneMoreXThanO (from m)
      _ -> False
  satisfiesProperty WinDeclared m = declare m

instance HasPermutationGenerator TicTacToeProperty TicTacToeMove where
  generators = []
--    let fromBoardSub = liftEdges FromBoardProperty
--                                 (trace "From1" from)
--                                 (trace "From2" (\f m -> m { from = f }))
--                                 (trace (show (logic :: Formula TicTacToeProperty))
--                                               (\p -> case p of
--                                                        (FromBoardProperty q) -> Just q
--                                                        _ -> Nothing))
--                                 "From"
--                                 generators
--        toBoardSub = liftEdges ToBoardProperty
--                               to
--                               (\f m -> m { to = f })
--                               (\p -> case p of
--                                        (ToBoardProperty q) -> Just q
--                                        _ -> Nothing)
--                               "To"
--                               generators
--     in join [fromBoardSub
--             ,toBoardSub
--             ]

instance HasParameterisedGenerator TicTacToeProperty TicTacToeMove where
  parameterisedGenerator = buildGen baseGen

baseGen :: Gen TicTacToeMove
baseGen = do
    let genTile = fromIntegral <$> Gen.int (linear minBound maxBound)
        genBoard = Gen.list (linear 0 100) genTile
        genPlayer = genTile
        genDeclare = Gen.bool
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

