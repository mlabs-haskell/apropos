module Main ( main ) where
import Test.Tasty
import Spec.Int

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ intGenTests
    , intDeviceTests
    ]
