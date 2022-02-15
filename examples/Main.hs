module Main ( main ) where
import Test.Tasty
import Spec.Int
import Spec.IntPermutingGen

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ intGenTests
    , intDeviceTests
    , intPermutingGenTests
    , intPermutingGenDeviceTests
    ]
