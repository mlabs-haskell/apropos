module Main ( main ) where
import Test.Tasty
import Spec.Int
import Spec.IntPermutationGen
import Spec.IntPair
import Spec.TicTacToe.Player
import Spec.TicTacToe.PlayerSequence
import Spec.TicTacToe.Location
import Spec.TicTacToe.LocationSequence
import Spec.TicTacToe.PlayerLocationSequencePair
import Spec.TicTacToe.Move
import Spec.TicTacToe.MoveSequence
import Spec.Plutarch.MagicNumber

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "all tests"
    [ testGroup "Int model Hand Written Parameterised Generator"
        [ intGenTests
        , intPureTests
        , intPlutarchTests
        ]
    , testGroup "Int model using Permutation Generator"
        [ intPermutationGenTests
        , intPermutationGenPureTests
        , intPermutationGenPlutarchTests
        , intPermutationGenSelfTests
        ]
    , testGroup "IntPair composite model"
        [ intPairGenSelfTests
        , intPairGenSelfTests
        , intPairGenPureTests
        , intPairGenPlutarchTests
        ]
    , testGroup "TicTacToe"
        [ playerPermutationGenSelfTest
        , locationPermutationGenSelfTest
        , movePermutationGenSelfTest
        , playerSequencePermutationGenSelfTest
        , locationSequencePermutationGenSelfTest
        , playerLocationSequencePairPermutationGenSelfTest
        , moveSequenceGenTests
        ]
    , testGroup "Script As Object"
        [ magicNumberPropGenTests
        ]
    ]
