module Apropos.Gen (
  Gen,
  Gen',
  GenException,
  genProp,
  genRoot,
  handleRootRetries,

  source,
  sink,
  label,
  failWithFootnote,
  liftEdge,
  liftGen,
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
import Control.Monad.Free
import Control.Monad (void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Except (ExceptT,throwE,runExceptT)
import Hedgehog (PropertyT,Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as HGen
import qualified Hedgehog.Range as HRange
import Data.String (fromString)

data Range = Linear Int Int | Singleton Int

linear :: Int -> Int -> Range
linear a b = Linear a b

singleton :: Int -> Range
singleton s = Singleton s

--rangeSize :: Range -> Int
--rangeSize (Singleton _) = 1
--rangeSize (Linear lo hi) = 1 + fromIntegral (max 0 (hi - lo))
--
--rangeHi :: Range -> Int
--rangeHi (Singleton s) = fromIntegral s
--rangeHi (Linear _ hi) = fromIntegral hi
--
--rangeLo :: Range -> Int
--rangeLo (Singleton s) = fromIntegral s
--rangeLo (Linear lo _) = fromIntegral lo

hRange :: Range -> H.Range Int
hRange (Singleton s) = HRange.singleton s
hRange (Linear lo hi) = HRange.linear lo hi

data FreeGen m next where
  ReadGenInput :: forall m next . (m -> next)  -> FreeGen m next
  LiftEdge :: forall m n next . Gen n n -> n -> (n -> next) -> FreeGen m next
  LiftGen :: forall m n next . Gen n n -> (n -> next) -> FreeGen m next
  Label :: String -> next -> FreeGen m next
  FailWithFootnote :: forall a m next . String -> (a -> next) -> FreeGen m next
  GenBool :: (Bool -> next) -> FreeGen m next
  GenInt :: Range -> (Int -> next) -> FreeGen m next
  GenList :: forall m a next .
    Range -> (Gen m a) -> ([a] -> next) -> FreeGen m next
  GenShuffle :: forall m a next . Show a =>
    [a] -> ([a] -> next) -> FreeGen m next
  GenElement :: forall m a next . Show a =>
    [a] -> (a -> next) -> FreeGen m next
  GenChoice :: forall m a next .
    [Gen m a] -> (a -> next) -> FreeGen m next
  GenFilter :: forall a m next . Show a =>
    (a -> Bool) -> (Gen m a) -> (a -> next) -> FreeGen m next
  RootRetry :: forall a m next . (a -> next) -> FreeGen m next

instance Functor (FreeGen m) where
  fmap f (ReadGenInput next) = ReadGenInput (f . next)
  fmap f (LiftEdge e i next) = LiftEdge e i (f . next)
  fmap f (LiftGen g next) = LiftGen g (f . next)
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


type Gen p = Free (FreeGen p)

type Gen' p = Gen p p


sink :: a -> Gen m a
sink = pure

source :: Gen m m
source = liftF (ReadGenInput id)

liftEdge :: Gen n n -> n -> Gen m n
liftEdge e i = liftF (LiftEdge e i id)

liftGen :: Gen n n -> Gen m n
liftGen g = liftF (LiftGen g id)

label :: String -> Gen m ()
label s = liftF (Label s ())

failWithFootnote :: String -> Gen m a
failWithFootnote s = liftF (FailWithFootnote s id)

bool :: Gen m Bool
bool = liftF (GenBool id)

int :: Range -> Gen m Int
int r = liftF (GenInt r id)

list :: Range -> Gen m a -> Gen m [a]
list r g = liftF (GenList r g id)

shuffle :: Show a => [a] -> Gen m [a]
shuffle l = liftF (GenShuffle l id)

element :: Show a => [a] -> Gen m a
element l = liftF (GenElement l id)

choice :: [Gen m a] -> Gen m a
choice g = liftF (GenChoice g id)

genFilter :: Show a => (a -> Bool) -> Gen m a -> Gen m a
genFilter c g = liftF (GenFilter c g id)

retry :: Gen m a
retry = liftF (RootRetry id)

genProp :: Gen m a -> Property
genProp g = H.property $ void $ runExceptT (genRoot g)

genRoot :: Gen m a -> ExceptT GenException (PropertyT IO) a
genRoot g = runReaderT (gen g) Nothing

data GenException = GenException String | Retry deriving stock (Show)

type GenContext = PropertyT IO

type Generator = ExceptT GenException GenContext

type GenMorphism m a = ReaderT (Maybe m) Generator a

gen :: forall m a . Gen m a -> GenMorphism m a
gen (Free (ReadGenInput next)) = do
  m <- ask
  case m of
    Nothing -> lift $ throwE $ GenException "can't read input in root gen"
    Just so -> gen $ next so
gen (Free (LiftEdge e i next)) = lift (genEdge e i) >>>= next
gen (Free (LiftGen g next)) = lift (genRoot g) >>>= next
gen (Free (Label s next)) = H.label (fromString s) >> gen next
gen (Free (FailWithFootnote s _)) = H.footnote s >> H.failure
gen (Free (GenBool next)) = (lift $ lift $ H.forAll $ HGen.bool) >>= (gen . next)
gen (Free (GenInt r next)) = do
  i <- lift $ lift (H.forAll $ HGen.int (hRange r))
  gen $ next i
gen (Free (GenList r g next)) = do
  let gs = int r >>= \l -> sequence $ replicate l g
  gen gs >>>= next
gen (Free (GenShuffle ls next)) = do
  s <- lift $ lift $ H.forAll $ HGen.shuffle ls
  gen $ next s
gen (Free (GenElement ls next)) = do
  s <- lift $ lift $ H.forAll $ HGen.element ls
  gen $ next s
gen (Free (GenChoice gs next)) = do
  let l = length gs
  if l == 0
     then lift $ throwE $ GenException "GenChoice: list length zero"
     else do
       i <- lift $ lift (H.forAll $ HGen.int (HRange.linear 0 (l-1)))
       (gs!!i) >>== next
gen (Free (GenFilter c g next)) = do
  genFilter' c g >>>= next
gen (Free (RootRetry _)) = lift $ throwE Retry
gen (Pure a) = pure a


(>>==) :: Gen m r -> (r -> Gen m a) -> GenMorphism m a
(>>==) a b = do
  m <- ask
  r <- lift $ lift (runExceptT (runReaderT (gen a) m))
  case r of
    Right x -> gen $ b x
    Left e -> lift $ throwE e

(>>>=) :: GenMorphism m r -> (r -> Gen m a) -> GenMorphism m a
(>>>=) a b = do
  m <- ask
  r <- lift $ lift (runExceptT (runReaderT a m))
  case r of
    Right x -> gen $ b x
    Left e -> lift $ throwE e

handleRootRetries :: Int -> Generator a -> GenContext a
handleRootRetries 0 _ = H.footnote ("global retry limit reached reached") >> H.failure
handleRootRetries l g = do
  gr <- runExceptT g
  case gr of
    Right a -> pure a
    Left Retry -> handleRootRetries (l-1) g
    Left (GenException e) -> H.footnote e >> H.failure

genEdge :: Gen m a -> m -> Generator a
genEdge g m = runReaderT (gen g) (Just m)

genFilter' :: forall m r. (r -> Bool) -> Gen m r -> GenMorphism m r
genFilter' condition g = go 100
  where
    go :: Int -> GenMorphism m r
    go l = do
      if l < 0
        then lift $ throwE Retry
        else do
          res <- gen g
          if condition res
            then pure res
            else go (l -1)

