--{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Apropos.LogicalModel.Formula (
  Formula (..),
  solveAll,
  satisfiable,
) where

import Data.Map
--import GHC.Generics (Generic)
import SAT.MiniSat qualified as S

infixr 6 :&&:
infixr 5 :||:
infixr 4 :++:
infixr 2 :->:
infix 1 :<->:

data Formula v where
  Var :: v -> Formula v
  Yes :: Formula v
  No :: Formula v
  Not :: Formula v -> Formula v
  (:&&:) :: Formula v -> Formula v -> Formula v
  (:||:) :: Formula v -> Formula v -> Formula v
  (:++:) :: Formula v -> Formula v -> Formula v
  (:->:) :: Formula v -> Formula v -> Formula v
  (:<->:) :: Formula v -> Formula v -> Formula v
  All :: [Formula v] -> Formula v
  Some :: [Formula v] -> Formula v
  None :: [Formula v] -> Formula v
  ExactlyOne :: [Formula v] -> Formula v
  AtMostOne :: [Formula v] -> Formula v
  Let :: Formula a -> (forall b. (a -> b) -> (Formula b -> Formula b)) -> Formula a
  Bound :: Integer -> Formula v

instance Functor Formula where
  fmap :: (a -> b) -> Formula a -> Formula b
  fmap _ Yes = Yes
  fmap _ No = No
  fmap f (Var x) = Var $ f x
  fmap f (Not x) = Not (f <$> x)
  fmap f (l :&&: r) = (f <$> l) :&&: (f <$> r)
  fmap f (l :||: r) = (f <$> l) :||: (f <$> r)
  fmap f (l :++: r) = (f <$> l) :++: (f <$> r)
  fmap f (l :->: r) = (f <$> l) :->: (f <$> r)
  fmap f (l :<->: r) = (f <$> l) :<->: (f <$> r)
  fmap f (All xs) = All (fmap f <$> xs)
  fmap f (Some xs) = Some (fmap f <$> xs)
  fmap f (None xs) = None (fmap f <$> xs)
  fmap f (ExactlyOne xs) = ExactlyOne (fmap f <$> xs)
  fmap f (AtMostOne xs) = AtMostOne (fmap f <$> xs)
  fmap f (Let base makeTemplate) = Let (f <$> base) (\g -> makeTemplate (g . f))
  fmap _ (Bound n) = Bound n

translateToSAT :: Formula v -> S.Formula v
translateToSAT (Var v) = S.Var v
translateToSAT Yes = S.Yes
translateToSAT No = S.No
translateToSAT (Not c) = S.Not (translateToSAT c)
translateToSAT (a :&&: b) = translateToSAT a S.:&&: translateToSAT b
translateToSAT (a :||: b) = translateToSAT a S.:||: translateToSAT b
translateToSAT (a :++: b) = translateToSAT a S.:++: translateToSAT b
translateToSAT (a :->: b) = translateToSAT a S.:->: translateToSAT b
translateToSAT (a :<->: b) = translateToSAT a S.:<->: translateToSAT b
translateToSAT (All cs) = S.All (translateToSAT <$> cs)
translateToSAT (Some cs) = S.Some (translateToSAT <$> cs)
translateToSAT (None cs) = S.None (translateToSAT <$> cs)
translateToSAT (ExactlyOne cs) = S.ExactlyOne (translateToSAT <$> cs)
translateToSAT (AtMostOne cs) = S.AtMostOne (translateToSAT <$> cs)
translateToSAT (Let x f) = S.Let (translateToSAT x) (translateToSAT . f id . translateFromSAT)
translateToSAT (Bound n) = S.Bound n

translateFromSAT :: S.Formula v -> Formula v
translateFromSAT (S.Var v) = Var v
translateFromSAT S.Yes = Yes
translateFromSAT S.No = No
translateFromSAT (S.Not c) = Not (translateFromSAT c)
translateFromSAT (a S.:&&: b) = translateFromSAT a :&&: translateFromSAT b
translateFromSAT (a S.:||: b) = translateFromSAT a :||: translateFromSAT b
translateFromSAT (a S.:++: b) = translateFromSAT a :++: translateFromSAT b
translateFromSAT (a S.:->: b) = translateFromSAT a :->: translateFromSAT b
translateFromSAT (a S.:<->: b) = translateFromSAT a :<->: translateFromSAT b
translateFromSAT (S.All cs) = All (translateFromSAT <$> cs)
translateFromSAT (S.Some cs) = Some (translateFromSAT <$> cs)
translateFromSAT (S.None cs) = None (translateFromSAT <$> cs)
translateFromSAT (S.ExactlyOne cs) = ExactlyOne (translateFromSAT <$> cs)
translateFromSAT (S.AtMostOne cs) = AtMostOne (translateFromSAT <$> cs)
translateFromSAT (S.Let x f) = Let (translateFromSAT x) (\labeler form -> undefined form labeler f)
  --Let :: Formula a -> (forall b. (a -> b) -> (Formula b -> Formula b)) -> Formula a
translateFromSAT (S.Bound n) = Bound n

instance (Eq v) => Eq (Formula v) where
  a == b = translateToSAT a == translateToSAT b

instance (Ord v) => Ord (Formula v) where
  compare a b = compare (translateToSAT a) (translateToSAT b)

instance (Show v) => Show (Formula v) where
  show a = show (translateToSAT a)

satisfiable :: Ord v => Formula v -> Bool
satisfiable = S.satisfiable . translateToSAT

solveAll :: Ord v => Formula v -> [Map v Bool]
solveAll = S.solve_all . translateToSAT
