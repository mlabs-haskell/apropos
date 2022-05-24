{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Apropos.Logic (
  Formula (..),
  solveAll,
  enumerateSolutions,
  enumerateScenariosWhere,
  scenarios,
  scenarioMap,
  satisfiedBy,
  satisfiesFormula,
  satisfiable,
  Strategy (..),
  satisfiesExpression,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import SAT.MiniSat qualified as S

infixr 6 :&&:
infixr 5 :||:
infixr 4 :++:
infixr 2 :->:
infix 1 :<->:

data Formula v
  = Var v
  | Yes
  | No
  | Not (Formula v)
  | Formula v :&&: Formula v
  | Formula v :||: Formula v
  | Formula v :++: Formula v
  | Formula v :->: Formula v
  | Formula v :<->: Formula v
  | All [Formula v]
  | Some [Formula v]
  | None [Formula v]
  | ExactlyOne [Formula v]
  | AtMostOne [Formula v]
  deriving stock (Generic, Functor)

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

enumerateSolutions :: (Ord v) => Formula v -> [Set v]
enumerateSolutions f = Map.keysSet . Map.filter id <$> solveAll f

enumerateScenariosWhere :: forall v a. (Ord v, Strategy v a) => Formula v -> [Set v]
enumerateScenariosWhere holds = enumerateSolutions @v $ logic :&&: holds

scenarios :: forall v a. (Ord v, Strategy v a) => [Set v]
scenarios = enumerateScenariosWhere Yes

scenarioMap :: (Ord v, Strategy v a) => Map Int (Set v)
scenarioMap = Map.fromList $ zip [0 ..] scenarios

satisfiedBy :: (Ord v, Strategy v a) => [v]
satisfiedBy = Set.toList $
  case scenarios of
    [] -> error "no solutions found for model logic"
    (sol : _) -> sol

satisfiesFormula :: forall v a. (Ord v, Strategy v a) => Formula v -> Set v -> Bool
satisfiesFormula f s = satisfiable $ f :&&: All (Var <$> set) :&&: None (Var <$> unset)
  where
    set :: [v]
    set = Set.toList s
    unset :: [v]
    unset = filter (`notElem` s) universe

class Strategy v a | v -> a where
  logic :: Formula v
  universe :: [v]
  variablesSet :: a -> Set v

satisfiesExpression :: (Strategy v a, Ord v) => Formula v -> a -> Bool
satisfiesExpression f m = satisfiesFormula f (variablesSet m)
