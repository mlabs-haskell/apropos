{-# LANGUAGE RankNTypes #-}
module Apropos.Gen (
  FreeGen (..),
  Gen,
  GenException (..),
  forAll,
  forAllWith,
  forAllRetryToMaybeScale,
  errorHandler,
  label,
  failWithFootnote,
  bool,
  int,
  list,
  shuffle,
  element,
  choice,
  genFilter,
  scale,
  prune,
  liftPropertyT,
  retry,
  onRetry,
  retryLimit,
  (===),
  Range,
  linear,
  linearFrom,
  singleton,
) where

import Apropos.Gen.Range
import Control.Monad (replicateM)
import Control.Monad.Free
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Hedgehog (PropertyT)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as HGen
import Hedgehog.Range qualified as HRange

forAllRetryToMaybeScale :: Show a => Gen a -> Int -> PropertyT IO (Maybe a)
forAllRetryToMaybeScale g s = do
  ee <- forAll $ scale (2 * s +) g
  case ee of
    Left Retry -> pure Nothing
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure $ Just a

forAllWithRetries :: forall a . Show a => Int -> Gen a -> PropertyT IO (Either GenException a)
forAllWithRetries retries g = go 0
  where
    go :: Int -> PropertyT IO (Either GenException a)
    go l = do
      res <- forAllRetryToMaybeScale g l
      case res of
        Nothing ->
          if l > retries
            then pure $ Left Retry
            else go (l + 1)
        Just so -> pure $ Right so


errorHandler :: Either GenException a -> PropertyT IO a
errorHandler ee =
  case ee of
    Left Retry -> H.footnote "retry limit reached" >> H.discard
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure a

forAll :: Show a => Gen a -> PropertyT IO (Either GenException a)
forAll = interleaved . gen2Interleaved

forAllWith :: forall a. (a -> String) -> Gen a -> PropertyT IO (Either GenException a)
forAllWith s = interleavedWith s . gen2Interleaved

forAllWithG :: forall a . (a -> String) -> Generator a -> PropertyT IO (Either GenException a)
forAllWithG s g = do
  (ee, labels) <- H.forAllWith sh $ runWriterT (runExceptT g)
  mapM_ (H.label . fromString) labels
  pure ee
  where
    sh :: (Either GenException a, Set String) -> String
    sh (Left e, _) = show e
    sh (Right a, _) = s a


interleaved :: forall a . Show a => Interleaved a -> PropertyT IO (Either GenException a)
interleaved = interleavedWith show

interleavedWith :: forall a . (a -> String) -> Interleaved a -> PropertyT IO (Either GenException a)
interleavedWith s m = do
  res <- forAllWithG sh $ gSteps m
  case res of
    Right (Right a) -> pure $ Right a
    Right (Left c) -> do
      pres <- pSteps c
      case pres of
        Right a -> pure $ Right a
        Left pc -> interleavedWith s pc
    Left err -> pure $ Left err
  where
    sh :: Either (Interleaved a) a -> String
    sh (Left _) = "This is unexpected. Generator interleaving failed. Please contact a customer support representative."
    sh (Right a) = s a


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
  Prune :: forall a next. Gen a -> (a -> next) -> FreeGen next
  LiftPropertyT :: forall a next. PropertyT IO a -> (a -> next) -> FreeGen next
  ThrowRetry :: forall a next. (a -> next) -> FreeGen next
  OnRetry :: forall a next. Show a => Gen a -> Gen a -> (a -> next) -> FreeGen next

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
  fmap f (Prune g next) = Prune g (f . next)
  fmap f (LiftPropertyT p next) = LiftPropertyT p (f . next)
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

prune :: Gen a -> Gen a
prune g = liftF (Prune g id)

liftPropertyT :: PropertyT IO a -> Gen a
liftPropertyT p = liftF (LiftPropertyT p id)

retry :: Gen a
retry = liftF (ThrowRetry id)

onRetry :: Show a => Gen a -> Gen a -> Gen a
onRetry a b = liftF (OnRetry a b id)

retryLimit :: forall a. Show a => Int -> Gen a -> Gen a -> Gen a
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

data FreeInterleaved next where
  G :: forall a next. Generator a -> (a -> next) -> FreeInterleaved next
  P :: forall a next. PropertyT IO a -> (a -> next) -> FreeInterleaved next


instance Functor FreeInterleaved where
  fmap f (G g next) = G g (fmap f next)
  fmap f (P p next) = P p (fmap f next)

type Interleaved = Free FreeInterleaved

mapG :: (forall a . Generator a -> Generator a) -> Interleaved b -> Interleaved b
mapG m (Free (G g next)) = Free (G (m g) (mapG m <$> next))
mapG m (Free (P p next)) = Free (P p (mapG m <$> next))
mapG _ (Pure r) = Pure r

gStep :: Interleaved a -> Generator (Either (Maybe (Interleaved a)) a)
gStep (Pure a) = pure $ Right a
gStep (Free (G g f)) = Left . Just . f <$> g
gStep _ = pure $ Left Nothing

gSteps :: Interleaved a -> Generator (Either (Interleaved a) a)
gSteps m = do
  mg <- gStep m
  case mg of
    Right a -> pure $ Right a
    Left Nothing -> pure $ Left m
    Left (Just so) -> gSteps so

gStepsA :: Interleaved a -> Generator (Interleaved a)
gStepsA m = do
  mg <- gStep m
  case mg of
    Right a -> pure $ Pure a
    Left Nothing -> pure  m
    Left (Just so) -> gStepsA so


pStep :: Interleaved a -> PropertyT IO (Either (Maybe (Interleaved a)) a)
pStep (Pure a) = pure $ Right a
pStep (Free (P g f)) = Left . Just . f <$> g
pStep _ = pure $ Left Nothing

pSteps :: Interleaved a -> PropertyT IO (Either (Interleaved a) a)
pSteps m = do
  mg <- pStep m
  case mg of
    Right a -> pure $ Right a
    Left Nothing -> pure $ Left m
    Left (Just so) -> pSteps so

liftG :: Generator a -> Interleaved a
liftG g = liftF (G g id)

liftP :: PropertyT IO a -> Interleaved a
liftP p = liftF (P p id)

gen2Interleaved :: Gen a -> Interleaved a
gen2Interleaved (Free (LiftPropertyT p next)) = liftP p >>= (gen2Interleaved . next)
gen2Interleaved (Free (Label s next)) = liftG (lift (tell (Set.singleton s))) >> gen2Interleaved next
gen2Interleaved (Free (FailWithFootnote s _)) = liftG $ throwE $ GenException s
gen2Interleaved (Free (GenBool next)) = liftG (lift HGen.bool) >>= (gen2Interleaved . next)
gen2Interleaved (Free (GenInt r next)) = do
  i <- liftG $ lift $ HGen.int (hRange r)
  gen2Interleaved $ next i
gen2Interleaved (Free (GenList r g next)) = do
  let gs = int r >>= \l -> replicateM l g
  gs >>>= next
gen2Interleaved (Free (GenShuffle ls next)) = do
  s <- liftG $ lift $ HGen.shuffle ls
  gen2Interleaved $ next s
gen2Interleaved (Free (GenElement ls next)) = do
  if null ls
    then liftG $ throwE $ GenException "GenElement empty list"
    else do
      s <- liftG $ lift $ HGen.element ls
      gen2Interleaved $ next s
gen2Interleaved (Free (GenChoice gs next)) = do
  let l = length gs
  if l == 0
    then liftG $ throwE $ GenException "GenChoice: list length zero"
    else do
      i <- liftG $ lift (HGen.int (HRange.linear 0 (l - 1)))
      (gs !! i) >>>= next
gen2Interleaved (Free (GenFilter c g next)) = do
  let f = do
           res <- g
           if c res
              then pure res
              else retry
  res <- liftP $ forAllWithRetries 100 f
  case res of
    Left err -> liftG (throwE err)
    Right a -> gen2Interleaved $ next a
gen2Interleaved (Free (Scale s g next)) = do
  sr <- mapG (HGen.scale (\(HRange.Size x) -> HRange.Size (s x))) (liftG (gStepsA (gen2Interleaved g)))
  sr >>= gen2Interleaved . next
gen2Interleaved (Free (Prune g next)) = do
  gr <- mapG HGen.prune $ liftG (gStepsA (gen2Interleaved g))
  gr >>= gen2Interleaved . next
gen2Interleaved (Free (ThrowRetry _)) = liftG $ throwE Retry
gen2Interleaved (Free (OnRetry a b next)) = do
  res <- liftP $ forAll a
  case res of
    Right r -> gen2Interleaved $ next r
    Left Retry -> gen2Interleaved b >>= (gen2Interleaved . next)
    Left err -> liftG $ throwE err
gen2Interleaved (Pure a) = pure a

(>>>=) :: Gen r -> (r -> Gen a) -> Interleaved a
(>>>=) a b = gen2Interleaved a >>= gen2Interleaved . b

hRange :: Range -> H.Range Int
hRange (Singleton s) = HRange.singleton s
hRange (Linear lo hi) = HRange.linear lo hi
hRange (LinearFrom mid lo hi) = HRange.linearFrom mid lo hi
