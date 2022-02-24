module Main (main) where

import Spec.Int
import Spec.IntPair
import Spec.IntPermutationGen
import Spec.Plutarch.CostModel
import Spec.Plutarch.MagicNumber
import Spec.TicTacToe.Location
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.Move
import Spec.TicTacToe.MoveSequence
import Spec.TicTacToe.Player
import Spec.TicTacToe.PlayerLocationSequencePair
import Spec.TicTacToe.PlayerSequence
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup
        "Int model Hand Written Parameterised Generator"
        [ intGenTests
        , intPureTests
        , intPlutarchTests
        ]
    , testGroup
        "Int model using Permutation Generator"
        [ intPermutationGenTests
        , intPermutationGenPureTests
        , intPermutationGenPlutarchTests
        , intPermutationGenSelfTests
        ]
    , testGroup
        "IntPair composite model"
        [ intPairGenSelfTests
        , intPairGenSelfTests
        , intPairGenPureTests
        , intPairGenPlutarchTests
        ]
    , testGroup
        "TicTacToe"
        [ playerPermutationGenSelfTest
        , locationPermutationGenSelfTest
        , movePermutationGenSelfTest
        , playerSequencePermutationGenSelfTest
        , locationSequencePermutationGenSelfTest
        , playerLocationSequencePairPermutationGenSelfTest
        , moveSequencePermutationGenSelfTest
        ]
    , testGroup
        "Script As Object"
        [ magicNumberPropGenTests
        , addCostPropGenTests
        , addCostModelPlutarchTests
        ]
    ]
