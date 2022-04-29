module Apropos.Gen.BacktrackingTraversal (
  Traversal (..),
  traversalAsGen,
  traversalContainRetry,
) where

import Apropos.Gen
import Apropos.HasPermutationGenerator.Morphism
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (runWriterT)
import Data.String (fromString)
import Hedgehog (PropertyT)
import Hedgehog qualified as H

data Traversal p m where
  FromSource :: Gen m -> Traversal p m
  Traversal :: Traversal p m -> (m -> Gen [Morphism p m]) -> Traversal p m

traversalAsGen :: Traversal p m -> Gen m
traversalAsGen (FromSource g) = g
traversalAsGen (Traversal m g) = do
  n <- traversalAsGen m
  t <- g n
  foldl (>=>) pure (morphism <$> t) n

traversalContainRetry :: Show m => Int -> Traversal p m -> PropertyT IO m
traversalContainRetry retries m = do
  r <- traversalWithRetries retries m
  case r of
    Nothing -> H.footnote "retry limit reached" >> H.discard
    Just so -> pure so

traversalWithRetries :: forall p a. Show a => Int -> Traversal p a -> PropertyT IO (Maybe a)
traversalWithRetries retries m =
  backtrackingRetryTraversals retries (forAllRetryToMaybeScale s) (genTraversal <$> t)
  where
    st :: (Gen a, [a -> Gen [Morphism p a]])
    st = unTraversal m
    s :: Gen a
    s = fst st
    t :: [a -> Gen [Morphism p a]]
    t = snd st
    genTraversal :: (a -> Gen [Morphism p a]) -> (a -> PropertyT IO [Int -> a -> PropertyT IO (Maybe a)])
    genTraversal gt = \a -> do
      tr <- forAllWith (unwords . (name <$>)) $ prune (gt a)
      pure $ sizableTr <$> (morphism <$> tr)
    sizableTr :: (a -> Gen a) -> (Int -> a -> PropertyT IO (Maybe a))
    sizableTr g = \si a -> forAllRetryToMaybeScale (g a) si

unTraversal :: Traversal p a -> (Gen a, [a -> Gen [Morphism p a]])
unTraversal (FromSource s) = (s, [])
unTraversal (Traversal s t) = case unTraversal s of
  (s', t') -> (s', t' <> [t])

forAllRetryToMaybeScale :: Show a => Gen a -> Int -> PropertyT IO (Maybe a)
forAllRetryToMaybeScale g s = do
  (ee, labels) <- H.forAll $ runWriterT (runExceptT $ gen $ scale (2 * s +) g)
  mapM_ (H.label . fromString) labels
  case ee of
    Left Retry -> pure Nothing
    Left (GenException err) -> H.footnote err >> H.failure
    Right a -> pure $ Just a

backtrackingRetryTraversals ::
  forall a m.
  Monad m =>
  Int ->
  (Int -> m (Maybe a)) ->
  [a -> m [Int -> a -> m (Maybe a)]] ->
  m (Maybe a)
backtrackingRetryTraversals retries g trs = go 1
  where
    go :: Int -> m (Maybe a)
    go l = do
      res <- g l
      case res of
        Nothing ->
          if l > retries
            then pure Nothing
            else go (l + 1)
        Just so -> do
          res' <- continueTraversal so 1 trs
          case res' of
            Nothing ->
              if l > retries
                then pure Nothing
                else go (l + 1)
            Just so' -> pure $ Just so'
    continueTraversal :: a -> Int -> [a -> m [Int -> a -> m (Maybe a)]] -> m (Maybe a)
    continueTraversal s _ [] = pure $ Just s
    continueTraversal s l (t : ts) = do
      ts' <- t s
      res <- backtrackingRetryTraverse retries s ts'
      case res of
        Nothing -> pure Nothing
        Just so -> do
          res' <- continueTraversal so 1 ts
          case res' of
            Nothing ->
              if l > retries
                then pure Nothing
                else continueTraversal s (l + 1) (t : ts)
            Just so' -> pure $ Just so'

backtrackingRetryTraverse :: forall a m. Monad m => Int -> a -> [Int -> a -> m (Maybe a)] -> m (Maybe a)
backtrackingRetryTraverse _ s [] = pure $ Just s
backtrackingRetryTraverse retries s (t : ts) = go 1
  where
    go :: Int -> m (Maybe a)
    go l = do
      res <- t l s
      case res of
        Nothing ->
          if l > retries
            then pure Nothing
            else go (l + 1)
        Just so -> do
          res' <- backtrackingRetryTraverse retries so ts
          case res' of
            Nothing ->
              if l > retries
                then pure Nothing
                else go (l + 1)
            Just so' -> pure $ Just so'
