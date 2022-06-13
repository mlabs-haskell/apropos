module Main (main) where

import Spec.Int

import Spec.Description.IntCompact
import Spec.Description.IntSimple

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup
        "LogicalModel"
        [ testGroup
            "Int model Hand Written Parameterised Generator"
            [ intGenTests
            , intPureTests
            ]
        -- , testGroup
        --     "IntPair composite model"
        --     [ intPairGenTests
        --     -- , intPairGenSelfTests
        --     -- , intPairGenPureTests
        --     ]
        -- , testGroup
        --     "IntEither composite model"
        --     [ intEitherGenTests
        --     ]
        -- , testGroup
        --     "Rational model"
        --     [ -- ratGenSelfTests
        --       ratSampleTests
        --     ]
        -- , testGroup
        --     "TicTacToe"
        --     [ playerPermutationGenSelfTest
        --     , locationPermutationGenSelfTest
        --     , movePermutationGenSelfTest
        --     , playerSequencePermutationGenSelfTest
        --     , locationSequencePermutationGenSelfTest
        --     , playerLocationSequencePairPermutationGenSelfTest
        --     , moveSequencePermutationGenSelfTest
        --     ]
        ]
    , testGroup
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
