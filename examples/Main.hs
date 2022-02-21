module Main ( main ) where
import Test.Tasty
import Spec.Int
import Spec.IntPermutationGen
import Spec.IntPair

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
    ]
