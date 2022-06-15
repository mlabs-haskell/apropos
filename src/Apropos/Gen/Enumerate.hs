module Apropos.Gen.Enumerate (
  enumerate,
  Exhaustivity (..),
  exhaustivityLabel,
  runTest,
) where

import Apropos.Gen
import Apropos.Gen.Range
import Control.Monad (join, replicateM)
import Control.Monad.Free
import Data.List (permutations)
import Hedgehog (Property, property, withTests)

-- Gen can be interpreted as an exhaustive enumeration
enumerate :: Gen a -> [a]
enumerate (Free (Label _ next)) = enumerate next
enumerate (Free (FailWithFootnote s _)) = error s
enumerate (Free (GenBool next)) =
  join (enumerate (sequence (next <$> [True, False])))
enumerate (Free (GenInt r next)) =
  join (enumerate (sequence (next <$> [(rangeLo r) .. (rangeHi r)])))
enumerate (Free (GenList r g next)) = do
  l <- enumerate (int r)
  gs <- replicateM l (enumerate g)
  enumerate $ next gs
enumerate (Free (GenShuffle ls next)) =
  join (enumerate (sequence (next <$> permutations ls)))
enumerate (Free (GenElement ls next)) =
  join (enumerate (sequence (next <$> ls)))
enumerate (Free (GenChoice gs next)) = do
  (>>== next) =<< gs
enumerate (Free (GenFilter c g next)) = do
  filter c (enumerate g) >=>= next
enumerate (Free (ThrowRetry _)) = error "enumerate can't retry"
enumerate (Free OnRetry {}) = error "maybe it can..."
enumerate (Pure a) = pure a
enumerate _ = error "enumerate can't do that"

(>>==) :: Gen r -> (r -> Gen a) -> [a]
(>>==) a b = (enumerate . b) =<< enumerate a

(>=>=) :: [r] -> (r -> Gen a) -> [a]
(>=>=) a b = (enumerate . b) =<< a

data Exhaustivity = Probablistic | Exhaustive

exhaustivityLabel :: Exhaustivity -> String
exhaustivityLabel Probablistic = "probablistic"
exhaustivityLabel Exhaustive = "exhaustive"

runTest :: Exhaustivity -> Gen a -> (a -> Gen ()) -> Property
runTest ex gen comp =
  case ex of
    Probablistic -> runTest'
    Exhaustive -> withTests 1 runTest'
  where
    runTest' :: Property
    runTest' = property $ runGenModifiable test >>= errorHandler

    test :: GenModifiable ()
    test = forAll $ do
      case ex of
        Probablistic -> gen >>= comp
        Exhaustive -> sequence_ (comp <$> enumerate gen)
