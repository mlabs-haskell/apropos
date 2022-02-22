module Brutus.Gen (
  PAGen,
  PGen,
  retry,
  source,
  runGenPA,
  liftGenPA,
  liftGenP,
  list,
  listPA,
  filterPA,
  ) where
import Hedgehog (Gen,Range,PropertyT,forAll)
import Control.Monad.Trans (lift)
import qualified Hedgehog.Gen as Gen
import Control.Monad.Trans.Reader (ReaderT,runReaderT,ask)
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)

type PAGen m r = MaybeT (ReaderT m (PropertyT IO)) r
type PGen r = PropertyT IO r

retry :: PAGen m r
retry = fail "retry"

source :: PAGen m m
source = lift ask

runGenPA :: forall m r . PAGen m r -> m -> PGen (Maybe r)
runGenPA g m = runReaderT (runMaybeT g) m

liftGenPA :: Show r => Gen r -> PAGen m r
liftGenPA = lift . lift . forAll

liftGenP :: Show r => Gen r -> PGen r
liftGenP = forAll

list :: Range Int -> PGen r -> PAGen m [r]
list range gen = do
  l <- liftGenPA $ Gen.int range
  sequence $ replicate l (lift $ lift gen)


listPA :: Range Int -> PAGen m r -> PAGen m [r]
listPA range gen = do
  l <- liftGenPA $ Gen.int range
  sequence $ replicate l gen

filterPA :: forall m r . (r -> Bool) -> PAGen m r -> PAGen m r
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

