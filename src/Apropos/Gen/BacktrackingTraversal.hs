module Apropos.Gen.BacktrackingTraversal (
  Traversal (..),
  traversalAsGen,
  traversalInGen,
  traversalWithRetries
) where

import Apropos.Gen
import Apropos.HasPermutationGenerator.Morphism
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans (lift)

data Traversal p m where
  FromSource :: Gen m -> Traversal p m
  Traversal :: Traversal p m -> (m -> Gen [Morphism p m]) -> Traversal p m

traversalInGen :: Show m => Int -> Traversal p m -> Gen m
traversalInGen retries = liftGenModifiable . traversalWithRetries retries

traversalAsGen :: Traversal p m -> Gen m
traversalAsGen (FromSource g) = g
traversalAsGen (Traversal m g) = do
  n <- traversalAsGen m
  t <- g n
  foldl (>=>) pure (morphism <$> t) n

traversalWithRetries :: forall p a. Show a => Int -> Traversal p a -> GenModifiable a
traversalWithRetries retries m = do
  res <- backtrackingRetryTraversals retries (forAllScale s) (genTraversal <$> t)
  case res of
    Left e -> lift $ throwE e
    Right so -> pure so
  where
    st :: (Gen a, [a -> Gen [Morphism p a]])
    st = unTraversal m
    s :: Gen a
    s = fst st
    t :: [a -> Gen [Morphism p a]]
    t = snd st
    genTraversal :: (a -> Gen [Morphism p a]) -> (a -> GenModifiable [Int -> a -> GenModifiable (Either GenException a)])
    genTraversal gt = \a -> do
      tr <- forAllWith (unwords . (name <$>)) $ prune (gt a)
      pure $ sizableTr <$> (morphism <$> tr)
    sizableTr :: (a -> Gen a) -> (Int -> a -> GenModifiable (Either GenException a))
    sizableTr g = \si a -> forAllScale (g a) si

unTraversal :: Traversal p a -> (Gen a, [a -> Gen [Morphism p a]])
unTraversal (FromSource s) = (s, [])
unTraversal (Traversal s t) = case unTraversal s of
  (s', t') -> (s', t' <> [t])

backtrackingRetryTraversals ::
  forall a m.
  Monad m =>
  Int ->
  (Int -> m (Either GenException a)) ->
  [a -> m [Int -> a -> m (Either GenException a)]] ->
  m (Either GenException a)
backtrackingRetryTraversals retries g trs =
  backtrackingBind retries 1 g (\x -> continueTraversal x 1 trs)
  where
    continueTraversal :: a -> Int -> [a -> m [Int -> a -> m (Either GenException a)]] -> m (Either GenException a)
    continueTraversal s _ [] = pure $ Right s
    continueTraversal s l (t : ts) = do
      ts' <- t s
      res <- backtrackingRetryTraverse retries s ts'
      case res of
        Left ex -> pure $ Left ex
        Right so -> do
          res' <- continueTraversal so 1 ts
          case res' of
            Left Retry ->
              if l > retries
                then pure $ Left Retry
                else continueTraversal s (l + 1) (t : ts)
            Left ex -> pure $ Left ex
            Right so' -> pure $ Right so'

backtrackingRetryTraverse :: forall a m. Monad m => Int -> a -> [Int -> a -> m (Either GenException a)] -> m (Either GenException a)
backtrackingRetryTraverse _ s [] = pure $ Right s
backtrackingRetryTraverse retries s (t : ts) =
  backtrackingBind retries 1 (`t` s) (flip (backtrackingRetryTraverse retries) ts)

backtrackingBind :: Monad m => Int -> Int -> (Int -> m (Either GenException a)) -> (a -> m (Either GenException a)) -> m (Either GenException a)
backtrackingBind retries counter f g = do
  runExceptT
    ( do
        res <- ExceptT $ f counter
        ExceptT $ g res
    )
    >>= \case
      Right so -> pure $ Right so
      Left Retry
        | counter > retries -> pure $ Left Retry
        | otherwise -> backtrackingBind retries (counter + 1) f g
      Left err -> pure $ Left err

forAllScale :: Show a => Gen a -> Int -> GenModifiable (Either GenException a)
forAllScale g s = lift $ lift $ runGenModifiable $ forAll $ scale (2 * s +) g

