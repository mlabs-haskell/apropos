module Apropos.Formula (
  Formula (..),
  solveAll,
  enumerateSolutions,
  satisfiable,
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

enumerateSolutions :: (Ord v) => Formula v -> Set (Set v)
enumerateSolutions f = Set.fromList $ Map.keysSet . Map.filter id <$> solveAll f
