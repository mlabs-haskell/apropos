module Main ( main ) where
import Test.Tasty
import Spec.TicTacToe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ ticTacToeTests
    ]
