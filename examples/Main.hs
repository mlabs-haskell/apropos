module Main (main) where

import Spec.IntCompact
import Spec.IntSimple

import Test.Tasty
import Test.Tasty.Hedgehog (fromGroup)

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
            [ fromGroup intSimpleGenTests
            , fromGroup intSimplePureTests
            ]
        , testGroup
            "Compact Int types"
            [ fromGroup intCompactGenTests
            , fromGroup intCompactPureTests
            ]
        ]
    ]
