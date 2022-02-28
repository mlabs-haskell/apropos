module Apropos.Gen (
  FreeGen(..),
  Gen,
  GenException,
  genProp,
  handleRootRetries,
  gen,

  label,
  failWithFootnote,
  bool,
  int,
  list,
  shuffle,
  element,
  choice,

  genFilter,
  retry,

  Range,
  linear,
  singleton,
) where
import Apropos.Gen.Range
import Control.Monad.Free
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT,throwE,runExceptT)
import Hedgehog (PropertyT,Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HGen
import qualified Hedgehog.Range as HRange
import Data.String (fromString)

data FreeGen next where
  Label :: String -> next -> FreeGen next
  FailWithFootnote :: forall a next . String -> (a -> next) -> FreeGen next
  GenBool :: (Bool -> next) -> FreeGen next
  GenInt :: Range -> (Int -> next) -> FreeGen next
  GenList :: forall a next .
    Range -> (Gen a) -> ([a] -> next) -> FreeGen next
  GenShuffle :: forall a next . Show a =>
    [a] -> ([a] -> next) -> FreeGen next
  GenElement :: forall a next . Show a =>
    [a] -> (a -> next) -> FreeGen next
  GenChoice :: forall a next .
    [Gen a] -> (a -> next) -> FreeGen next
  GenFilter :: forall a next . Show a =>
    (a -> Bool) -> (Gen a) -> (a -> next) -> FreeGen next
  RootRetry :: forall a next . (a -> next) -> FreeGen next

instance Functor (FreeGen) where
  fmap f (Label l next) = Label l (f next)
  fmap f (FailWithFootnote l next) = FailWithFootnote l (f . next)
  fmap f (GenBool next) = GenBool (f . next)
  fmap f (GenInt r next) = GenInt r (f . next)
  fmap f (GenList r g next) = GenList r g (f . next)
  fmap f (GenShuffle l next) = GenShuffle l (f . next)
  fmap f (GenElement ls next) = GenElement ls (f . next)
  fmap f (GenChoice gs next) = GenChoice gs (f . next)
  fmap f (GenFilter c g next) = GenFilter c g (f . next)
  fmap f (RootRetry next) = RootRetry (f . next)

type Gen = Free FreeGen

label :: String -> Gen ()
label s = liftF (Label s ())

failWithFootnote :: String -> Gen a
failWithFootnote s = liftF (FailWithFootnote s id)

bool :: Gen Bool
bool = liftF (GenBool id)

int :: Range -> Gen Int
int r = liftF (GenInt r id)

list :: Range -> Gen a -> Gen [a]
list r g = liftF (GenList r g id)

shuffle :: Show a => [a] -> Gen [a]
shuffle l = liftF (GenShuffle l id)

element :: Show a => [a] -> Gen a
element l = liftF (GenElement l id)

choice :: [Gen a] -> Gen a
choice g = liftF (GenChoice g id)

genFilter :: Show a => (a -> Bool) -> Gen a -> Gen a
genFilter c g = liftF (GenFilter c g id)

retry :: Gen a
retry = liftF (RootRetry id)

genProp :: Gen a -> Property
genProp g = H.property $ void $ runExceptT $ gen g

data GenException = GenException String | Retry deriving stock (Show)

type GenContext = PropertyT IO

type Generator = ExceptT GenException GenContext

gen :: Gen a -> Generator a
gen (Free (Label s next)) = H.label (fromString s) >> gen next
gen (Free (FailWithFootnote s _)) = H.footnote s >> H.failure
gen (Free (GenBool next)) = (lift $ H.forAll $ HGen.bool) >>= (gen . next)
gen (Free (GenInt r next)) = do
  i <- lift (H.forAll $ HGen.int (hRange r))
  gen $ next i
gen (Free (GenList r g next)) = do
  let gs = int r >>= \l -> sequence $ replicate l g
  gen gs >>>= next
gen (Free (GenShuffle ls next)) = do
  s <- lift $ H.forAll $ HGen.shuffle ls
  gen $ next s
gen (Free (GenElement ls next)) = do
  s <- lift $ H.forAll $ HGen.element ls
  gen $ next s
gen (Free (GenChoice gs next)) = do
  let l = length gs
  if l == 0
     then throwE $ GenException "GenChoice: list length zero"
     else do
       i <- lift (H.forAll $ HGen.int (HRange.linear 0 (l-1)))
       (gs!!i) >>== next
gen (Free (GenFilter c g next)) = do
  genFilter' c g >>>= next
gen (Free (RootRetry _)) = throwE Retry
gen (Pure a) = pure a


(>>==) :: Gen r -> (r -> Gen a) -> Generator a
(>>==) a b = do
  r <- lift (runExceptT (gen a))
  case r of
    Right x -> gen $ b x
    Left e -> throwE e

(>>>=) :: Generator r -> (r -> Gen a) -> Generator a
(>>>=) a b = do
  r <- lift (runExceptT a)
  case r of
    Right x -> gen $ b x
    Left e -> throwE e

handleRootRetries :: Int -> Generator a -> GenContext a
handleRootRetries 0 _ = H.footnote ("global retry limit reached reached") >> H.failure
handleRootRetries l g = do
  gr <- runExceptT g
  case gr of
    Right a -> pure a
    Left Retry -> handleRootRetries (l-1) g
    Left (GenException e) -> H.footnote e >> H.failure

genFilter' :: forall r . (r -> Bool) -> Gen r -> Generator r
genFilter' condition g = go 100
  where
    go :: Int -> Generator r
    go l = do
      if l < 0
        then throwE Retry
        else do
          res <- gen g
          if condition res
            then pure res
            else go (l -1)

hRange :: Range -> H.Range Int
hRange (Singleton s) = HRange.singleton s
hRange (Linear lo hi) = HRange.linear lo hi


