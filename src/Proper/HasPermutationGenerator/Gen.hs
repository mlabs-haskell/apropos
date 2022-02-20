module Proper.HasPermutationGenerator.Gen (
  PAGen,
  PGen,
  liftGenPA,
  liftGenP,
  list,
  ) where
import Hedgehog (Gen,Range,PropertyT,forAll)
import Control.Monad.Trans (lift)
import qualified Hedgehog.Gen as Gen
import Control.Monad.Trans.Reader (ReaderT)


type PAGen m r = ReaderT m (PropertyT IO) r
type PGen r = PropertyT IO r


liftGenPA :: Show r => Gen r -> PAGen m r
liftGenPA = lift . forAll

liftGenP :: Show r => Gen r -> PGen r
liftGenP = forAll

list :: Range Int -> PGen r -> PAGen m [r]
list range gen = do
  l <- lift $ forAll $ Gen.int range
  sequence $ replicate l (lift gen)
