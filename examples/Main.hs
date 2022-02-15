module Main ( main ) where
import Test.Tasty
import Proper.Data.Int

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ intGenTests
    ]
