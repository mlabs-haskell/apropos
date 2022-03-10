module Apropos.Gen (
  FreeGen (..),
  Gen,
  GenException,
  forAll,
  handleRetries,
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
  onRetry,
  retryLimit,
  (===),
  Range,
  linear,
  singleton,
) where
--import Debug.Trace
import Apropos.Gen.Range
import Control.Monad (replicateM)
import Control.Monad.Free
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.String (fromString)
import Hedgehog (PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange

data FreeGen next where
  Label :: String -> next -> FreeGen next
  FailWithFootnote :: forall a next. String -> (a -> next) -> FreeGen next
  GenBool :: (Bool -> next) -> FreeGen next
  GenInt :: Range -> (Int -> next) -> FreeGen next
  GenList ::
    forall a next.
    Range ->
    Gen a ->
    ([a] -> next) ->
    FreeGen next
  GenShuffle ::
    forall a next.
    Show a =>
    [a] ->
    ([a] -> next) ->
    FreeGen next
  GenElement ::
    forall a next.
    Show a =>
    [a] ->
    (a -> next) ->
    FreeGen next
  GenChoice ::
    forall a next.
    [Gen a] ->
    (a -> next) ->
    FreeGen next
  GenFilter ::
    forall a next.
    Show a =>
    (a -> Bool) ->
    Gen a ->
    (a -> next) ->
    FreeGen next
  ThrowRetry :: forall a next. (a -> next) -> FreeGen next
  OnRetry :: forall a next. Gen a -> Gen a -> (a -> next) -> FreeGen next

instance Functor FreeGen where
  fmap f (Label l next) = Label l (f next)
  fmap f (FailWithFootnote l next) = FailWithFootnote l (f . next)
  fmap f (GenBool next) = GenBool (f . next)
  fmap f (GenInt r next) = GenInt r (f . next)
  fmap f (GenList r g next) = GenList r g (f . next)
  fmap f (GenShuffle l next) = GenShuffle l (f . next)
  fmap f (GenElement ls next) = GenElement ls (f . next)
  fmap f (GenChoice gs next) = GenChoice gs (f . next)
  fmap f (GenFilter c g next) = GenFilter c g (f . next)
  fmap f (ThrowRetry next) = ThrowRetry (f . next)
  fmap f (OnRetry a b next) = OnRetry a b (f . next)

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
retry = liftF (ThrowRetry id)

onRetry :: Gen a -> Gen a -> Gen a
onRetry a b = liftF (OnRetry a b id)

retryLimit :: forall a. Int -> Gen a -> Gen a -> Gen a
retryLimit lim g done = go lim
  where
    go :: Int -> Gen a
    go i =
      if i == 0
        then done
        else onRetry g (go (i - 1))

(===) :: (Eq a, Show a) => a -> a -> Gen ()
(===) l r =
  if l == r
    then pure ()
    else failWithFootnote $ "expected: " <> show l <> " === " <> show r

forAll :: Show a => Gen a -> PropertyT IO a
forAll g = do
  -- we may want Gen level prune
  -- this is probably good for edges but not for most uses
  (ee,labels) <- H.forAll $ HGen.prune $ runWriterT (runExceptT $ gen g)
  mapM_ (H.label . fromString) labels
  case ee of
    Left Retry -> H.footnote "global retry limit reached" >> H.failure
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure a

data GenException = GenException String | Retry deriving stock (Show)

type GenContext = H.Gen

type Generator = ExceptT GenException (WriterT [String] GenContext)

gen :: Gen a -> Generator a
gen (Free (Label s next)) = lift (tell [s]) >> gen next
gen (Free (FailWithFootnote s _)) = throwE $ GenException s
gen (Free (GenBool next)) = lift HGen.bool >>= (gen . next)
gen (Free (GenInt r next)) = do
  i <- lift $ HGen.int (hRange r)
  gen $ next i
gen (Free (GenList r g next)) = do
  let gs = int r >>= \l -> replicateM l g
  gen gs >>>= next
gen (Free (GenShuffle ls next)) = do
  s <- lift $ HGen.shuffle ls
  gen $ next s
gen (Free (GenElement ls next)) = do
  s <- lift $ HGen.element ls
  gen $ next s
gen (Free (GenChoice gs next)) = do
  let l = length gs
  if l == 0
    then throwE $ GenException "GenChoice: list length zero"
    else do
      i <- lift (HGen.int (HRange.linear 0 (l -1)))
      (gs !! i) >>== next
gen (Free (GenFilter c g next)) = do
  genFilter' c g >>>= next
gen (Free (ThrowRetry _)) = throwE Retry
gen (Free (OnRetry a b next)) = do
  res <- lift $ runExceptT (gen a)
  case res of
    Right r -> gen $ next r
    Left Retry -> gen b >>= (gen . next)
    Left err -> throwE err
gen (Pure a) = pure a

(>>==) :: Gen r -> (r -> Gen a) -> Generator a
(>>==) a b = gen a >>>= b
  {-
  do
  r <- lift (runExceptT (gen a))
  case r of
    Right x -> gen $ b x
    Left e -> throwE e
    -}

(>>>=) :: Generator r -> (r -> Gen a) -> Generator a
(>>>=) a b = do
  r <- lift (runExceptT a)
  case r of
    Right x -> gen $ b x
    Left e -> throwE e

handleRetries :: Int -> Gen a -> Gen a
handleRetries l g = retryLimit l g retry
  {-
  traceShow l $ do
  if l < 1
     then g
     else onRetry g (handleRetries (l - 1) g)
     -}


genFilter' :: forall r. (r -> Bool) -> Gen r -> Generator r
genFilter' condition g = HGen.filterT condition (gen g)
  {-
  go 100
  where
    go :: Int -> Gen r
    go 0 = retry
    go l = do
      res <- g
      if condition res
         then pure res
         else go (l-1)
         -}

hRange :: Range -> H.Range Int
hRange (Singleton s) = HRange.singleton s
hRange (Linear lo hi) = HRange.linear lo hi
