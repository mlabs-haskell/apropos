module Main ( main ) where
import Test.Tasty
import Spec.Int
import Spec.IntPermutationGen
import Spec.IntPair
import Spec.TicTacToe.Player
import Spec.TicTacToe.Tile
import Spec.TicTacToe.Board
import Spec.TicTacToe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup "Hand Written Parameterised Generator"
        [intGenTests
        , intPureTests
        , intPlutarchTests
        ]
    , testGroup "Permutation Generator"
        [ intPermutationGenTests
        , intPermutationGenPureTests
        , intPermutationGenPlutarchTests
        , intPermutationGenSelfTests
        ]
    , testGroup "IntPair composite model"
        [ intPairGenSelfTests
        , intPairGenSelfTests
        , intPairGenPureTests
        , intPairGenPlutarchTests
        ]
    , testGroup "TicTacToe"
        [ ticTacToePlayerGenSelfTests
        , ticTacToeTileGenSelfTests
        , ticTacToeBoardGenSelfTests
--        , ticTacToeGenTests
        , ticTacToeGenSelfTests
        ]
    ]
