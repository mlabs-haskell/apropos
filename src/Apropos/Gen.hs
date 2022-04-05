module Apropos.Gen (
  Morph(..),
  morphAsGen,
  morphContain,
  morphContainRetry,
  FreeGen (..),
  Gen,
  GenException,
  forAll,
  forAllWithRetries,
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
  freeze,
  scale,
  retry,
  onRetry,
  retryLimit,
  (===),
  Range,
  linear,
  singleton,
) where
import Apropos.Gen.Range
import Control.Monad (replicateM,(>=>),guard)
import Control.Monad.Free
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.String (fromString)
import Hedgehog (PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange
import Hedgehog.Internal.Tree qualified as HIT
import Hedgehog.Internal.Gen qualified as HIG
import Data.Set (Set)
import Data.Set qualified as Set


forAllWithRetries :: Show a => Int -> Gen a -> PropertyT IO a
forAllWithRetries lim g = do
  (ee,labels) <- H.forAll $ runWriterT (runExceptT $ handleRetries lim g)
  mapM_ (H.label . fromString) labels
  case ee of
    Left Retry -> H.footnote "retry limit reached" >> H.failure
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure a


forAllNoShrink :: Show a => Gen a -> PropertyT IO a
forAllNoShrink g = do
  (ee,labels) <- H.forAll $ HGen.prune $ runWriterT (runExceptT $ gen g)
  mapM_ (H.label . fromString) labels
  case ee of
    Left Retry -> H.footnote "retry limit reached" >> H.failure
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure a

forAll :: Show a => Gen a -> PropertyT IO a
forAll g = do
  (ee,labels) <- H.forAll $ runWriterT (runExceptT $ gen g)
  mapM_ (H.label . fromString) labels
  case ee of
    Left Retry -> H.footnote "retry limit reached" >> H.failure
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure a

data Morph m where
  Source :: Gen m -> Morph m
  Morph :: Morph m -> (m -> Gen [m -> Gen m]) -> Morph m

--TODO use Morphism instead then we can print the Morphism name
--i.e. Morph :: Morph p m -> (m -> Gen [Morphism p m]) -> Morph p m
--also Source has been introduced - we want to be able to partition the space with a set of sources! XD
instance Show (m -> Gen m) where
  show _ = "GenMorphism"

morphAsGen :: Morph m -> Gen m
morphAsGen (Source g) = g
morphAsGen (Morph m g) = do
  n <- morphAsGen m
  t <- g n
  foldl (>=>) pure t n

morphContain :: Show m => Morph m -> PropertyT IO m
morphContain (Source g) = forAll g
morphContain (Morph m g) = do
  n <- morphContain m
  t <- forAllNoShrink $ g n
  foldl (\a b -> a >>= forAll . b) (pure n) t

morphContainRetry :: Show m => Int -> Morph m -> PropertyT IO m
morphContainRetry retries m = do
  r <- morphContainRetry' retries m
  case r of
    Nothing -> H.footnote ("retry limit (" <> show retries <> ") reached") >> H.failure
    Just so -> pure so

-- hmmmmm - let's refactor Morph
morphContainRetry' :: Show m => Int -> Morph m -> PropertyT IO (Maybe m)
morphContainRetry' _ (Source g) = forAllRetryToMaybe g
morphContainRetry' retries (Morph m g) = do
  handleRetries' retries (morphContainRetry' retries m) $ \a -> do
    t <- forAllNoShrink $ g a
    let c = steps' retries t
    c a

-- naming here is not great
steps :: Int -> [a -> PropertyT IO (Maybe a)] -> (a -> PropertyT IO (Maybe a))
steps _ [] = pure . Just
steps retries (c:cs) = \x -> handleRetries' retries (c x) (steps retries cs)

-- TODO we want to pass in a scale parameter that scales each step on retry
steps' :: Show a => Int -> [a -> Gen a] -> (a -> PropertyT IO (Maybe a))
steps' retries gs = do
  let rs = (forAllRetryToMaybe .) <$> gs
  steps retries rs

forAllRetryToMaybe :: Show a => Gen a -> PropertyT IO (Maybe a)
forAllRetryToMaybe g = do
  (ee,labels) <- H.forAll $ runWriterT (runExceptT $ gen g)
  mapM_ (H.label . fromString) labels
  case ee of
    Left Retry -> pure Nothing
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure $ Just a


handleRetries' :: forall m a b. Monad m => Int -> m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
handleRetries' n g c = go n
  where
    go :: Int -> m (Maybe b)
    go l = do
      res <- g
      case res of
        Just r -> do
          res2 <- c r
          case res2 of
            Nothing -> go (l - 1)
            Just so -> pure $ Just so
        Nothing ->
          if l < 1
             then pure Nothing
             else go (l - 1)



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
  Scale :: forall a next. (Int -> Int) -> Gen a -> (a -> next) -> FreeGen next
  Freeze :: forall a next. Gen a -> ((a, Gen a) -> next) -> FreeGen next
  GenWrap :: forall a next. Generator a -> (a -> next) -> FreeGen next
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
  fmap f (Scale s g next) = Scale s g (f . next)
  fmap f (Freeze g next) = Freeze g (f . next)
  fmap f (GenWrap g next) = GenWrap g (f . next)
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

scale :: (Int -> Int) -> Gen a -> Gen a
scale s g = liftF (Scale s g id)

freeze :: Gen a -> Gen (a, Gen a)
freeze g = liftF (Freeze g id)

-- let's keep this internal
genWrap :: Generator a -> Gen a
genWrap g = liftF (GenWrap g id)

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

data GenException = GenException String | Retry deriving stock (Show)

type GenContext = H.Gen

type Generator = ExceptT GenException (WriterT (Set String) GenContext)

gen :: Gen a -> Generator a
gen (Free (Label s next)) = lift (tell (Set.singleton s)) >> gen next
gen (Free (FailWithFootnote s _)) = throwE $ GenException s
gen (Free (GenBool next)) = lift HGen.bool >>= (gen . next)
gen (Free (GenInt r next)) = do
  i <- lift $ HGen.int (hRange r)
  gen $ next i
gen (Free (GenList r g next)) = do
  let gs = int r >>= \l -> replicateM l g
  gen gs >=>= next
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
  res <- filter' c $ gen g
  gen $ next res
gen (Free (Scale s g next)) =
  HGen.scale (\(HRange.Size x) -> HRange.Size (s x)) (gen g) >>= gen . next
gen (Free (Freeze g next)) = do
  (x, gw) <- HGen.freeze (gen g)
  gen $ next (x, genWrap gw)
gen (Free (GenWrap g next)) = g >>= gen . next
gen (Free (ThrowRetry _)) = throwE Retry
gen (Free (OnRetry a b next)) = do
  res <- lift $ runExceptT (gen a)
  case res of
    Right r -> gen $ next r
    Left Retry -> gen b >>= (gen . next)
    Left err -> throwE err
gen (Pure a) = pure a

(>>==) :: Gen r -> (r -> Gen a) -> Generator a
(>>==) a b = do
  r <- lift (runExceptT (gen a))
  case r of
    Right x -> gen $ b x
    Left e -> throwE e

(>=>=) :: Generator r -> (r -> Gen a) -> Generator a
(>=>=) a b = do
  r <- lift (runExceptT a)
  case r of
    Right x -> gen $ b x
    Left e -> throwE e

handleRetries :: forall a . Int -> Gen a -> Generator a
handleRetries n g = go n
  where
    go :: Int -> Generator a
    go l = do
      res <- lift $ runExceptT (gen $ scale ((n - l) +) g)
      case res of
        Right r -> pure r
        Left Retry ->
          if l < 1
             then throwE Retry
             else go (l - 1)
        Left err -> throwE err


fromPred :: (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)

filter' :: (a -> Bool) -> Generator a -> Generator a
filter' p =
  mapMaybe (fromPred p)

mapMaybe :: forall a b . (a -> Maybe b) -> Generator a -> Generator b
mapMaybe p gen0 =
  let
    try k =
      if k > 100 then
        throwE Retry
      else do
        (x, g) <- lift $ lift $ HGen.freeze $ HGen.scale (2 * k +) (runWriterT (runExceptT gen0))
        case (p <$>) $ fst x of
          Right (Just _) -> do
            let withGenT f = H.fromGenT . f . H.toGenT
            let toMaybe' z = case fst z of
                               Right a -> a
                               Left _ -> error "This should be impossible."
            lift $ tell $ snd x
            lift $ lift $ withGenT (HIG.mapGenT (HIT.mapMaybeMaybeT p)) (toMaybe' <$> g)
          Left (GenException e) -> throwE $ GenException e
          _ -> try (k + 1)
  in
    try 0

hRange :: Range -> H.Range Int
hRange (Singleton s) = HRange.singleton s
hRange (Linear lo hi) = HRange.linear lo hi
