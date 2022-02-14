module Main ( main ) where
import Test.Tasty
import Spec.TicTacToe

main :: IO ()
main = doCheck >> defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ ticTacToeTests
    ]
