module Proper.HasPermutationGenerator.Gen (
  liftGen,
  list,
  ) where
import Hedgehog (Gen,Range,PropertyT,forAll)
import Control.Monad.Trans (MonadTrans,lift)
import qualified Hedgehog.Gen as Gen

liftGen :: forall m t mt . Monad t
        => MonadTrans mt
        => Show m
        => Gen m -> mt (PropertyT t) m
liftGen = lift . forAll

list :: forall mt m a . Monad (mt (PropertyT m))
     => MonadTrans mt
     => Monad m
     => Range Int -> PropertyT m a -> mt (PropertyT m) [a]
list range gen = do
  l <- lift $ forAll $ Gen.int range
  sequence $ replicate l (lift gen)
