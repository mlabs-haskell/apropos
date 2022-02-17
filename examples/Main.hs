module Main ( main ) where
import Test.Tasty
import Spec.Int
import Spec.IntPermutingGen
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
        [ intPermutingGenTests
        , intPermutingGenPureTests
        , intPermutingGenPlutarchTests
        , intPermutingGenSelfTests
        ]
    , testGroup "TicTacToe"
        [ ticTacToeGenSelfTests
        , ticTacToeGenTests
        ]
    ]
