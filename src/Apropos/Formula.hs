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

data Formula attr
  = Var attr
  | Yes
  | No
  | Not (Formula attr)
  | Formula attr :&&: Formula attr
  | Formula attr :||: Formula attr
  | Formula attr :++: Formula attr
  | Formula attr :->: Formula attr
  | Formula attr :<->: Formula attr
  | All [Formula attr]
  | Some [Formula attr]
  | None [Formula attr]
  | ExactlyOne [Formula attr]
  | AtMostOne [Formula attr]
  deriving stock (Generic, Functor)

translateToSAT :: Formula attr -> S.Formula attr
translateToSAT (Var attr) = S.Var attr
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

instance (Eq attr) => Eq (Formula attr) where
  a == b = translateToSAT a == translateToSAT b

instance (Ord attr) => Ord (Formula attr) where
  compare a b = compare (translateToSAT a) (translateToSAT b)

instance (Show attr) => Show (Formula attr) where
  show a = show (translateToSAT a)

satisfiable :: Ord attr => Formula attr -> Bool
satisfiable = S.satisfiable . translateToSAT

solveAll :: Ord attr => Formula attr -> [Map attr Bool]
solveAll = S.solve_all . translateToSAT

enumerateSolutions :: (Ord attr) => Formula attr -> Set (Set attr)
enumerateSolutions f = Set.fromList $ Map.keysSet . Map.filter id <$> solveAll f
