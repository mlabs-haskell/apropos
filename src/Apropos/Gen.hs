module Apropos.Gen (
  PAGen,
  PGen,
  source,
  runGenPA,
  liftGenPA,
  liftGenP,
  list,
  listPA,
  filterPA,
) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Hedgehog (Gen, PropertyT, Range, forAll)
import qualified Hedgehog.Gen as Gen

type PAGen m r = ReaderT m (PropertyT IO) r
type PGen r = PropertyT IO r

source :: PAGen m m
source = ask

runGenPA :: forall m r. PAGen m r -> m -> PGen r
runGenPA g m = runReaderT g m

liftGenPA :: Show r => Gen r -> PAGen m r
liftGenPA = lift . forAll

liftGenP :: Show r => Gen r -> PGen r
liftGenP = forAll

list :: Range Int -> PGen r -> PAGen m [r]
list range gen = do
  l <- liftGenPA $ Gen.int range
  sequence $ replicate l (lift gen)

listPA :: Range Int -> PAGen m r -> PAGen m [r]
listPA range gen = do
  l <- liftGenPA $ Gen.int range
  sequence $ replicate l gen

filterPA :: forall m r. (r -> Bool) -> PAGen m r -> PAGen m r
filterPA condition g = go 100
  where
    go :: Int -> PAGen m r
    go limit = do
      if limit < 0
        then error "filterPA: Tried 100 samples and rejected them all."
        else pure ()
      res <- g
      if condition res
        then pure res
        else go (limit -1)
