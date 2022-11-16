module Main (main) where

import Spec.IntCompact (
  intCompactAproposExample,
  intCompactExampleUnit,
  intCompactSelfTest,
 )
import Spec.IntSimple (
  intSimpleAproposExample,
  intSimpleBadProperty,
  intSimpleExampleUnit,
  intSimpleSelfTest,
 )

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Hedgehog (fromGroup, testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup
        "Simple Int types with logic"
        [ testProperty "Bad property test: this should fail!" intSimpleBadProperty
        , testCase "This is why:" intSimpleExampleUnit
        , fromGroup intSimpleSelfTest
        , fromGroup intSimpleAproposExample
        ]
    , testGroup
        "Compact Int types"
        [ fromGroup intCompactSelfTest
        , testCase "Failing example" intCompactExampleUnit
        , fromGroup intCompactAproposExample
        ]
    ]
