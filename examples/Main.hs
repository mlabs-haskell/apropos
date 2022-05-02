module Main (main) where

import Spec.Int
import Spec.IntEither
import Spec.IntPair
import Spec.IntPermutationGen
import Spec.Rational
import Spec.TicTacToe.Location
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.Move
import Spec.TicTacToe.MoveSequence
import Spec.TicTacToe.Player
import Spec.TicTacToe.PlayerLocationSequencePair
import Spec.TicTacToe.PlayerSequence
import Spec.IntOverlay
import Apropos.Overlay
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

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
        ]
    , testGroup
      "IntSmpl tests"
      [ testProperty "antiValidity check" (soundOverlay @IntSmpl)
      , intSmplPermutationGenTests
      ]
    , testGroup
        "Int model using Permutation Generator"
        [ intPermutationGenTests
        , intPermutationGenPureTests
        , intPermutationGenSelfTests
        ]
    , testGroup
        "IntPair composite model"
        [ intPairGenTests
        , intPairGenSelfTests
        , intPairGenPureTests
        ]
    , testGroup
        "IntEither composite model"
        [ intEitherGenTests
        ]
    , testGroup
        "Rational model"
        [ ratGenSelfTests
        , ratSampleTests
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
    ]
