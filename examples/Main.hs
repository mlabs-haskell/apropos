module Main (main) where

import Spec.IntCompact
import Spec.IntSimple

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup
        "Description"
        [ testGroup
            "Simple Int types with logic"
            [ intSimpleGenTests
            , intSimplePureTests
            ]
        , testGroup
            "Compact Int types"
            [ intCompactGenTests
            , intCompactPureTests
            ]
        ]
    ]
