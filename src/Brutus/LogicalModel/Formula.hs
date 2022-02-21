{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
module Brutus.LogicalModel.Formula (
  Formula(..),
  solve_all,
  satisfiable,
  ) where
import GHC.Generics (Generic)
import qualified SAT.MiniSat as S
import Data.Map

data Formula v =
    Var v
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
  deriving stock (Generic,Functor)

-- TODO review - is this lawful?
-- The solutions of (A <$> logic <*> logic) should be the cross product of the solutions
-- of the child models.
instance Applicative Formula where
  pure v = Var v
  (<*>) Yes _ = Yes
  (<*>) No _ = No
  (<*>) (Var a) b = a <$> b
  (<*>) (Not f) b = Not (f <*> b)
  (<*>) (a :&&: b) c = appBinf (:&&:) a b c
  (<*>) (a :||: b) c = appBinf (:||:) a b c
  (<*>) (a :++: b) c = appBinf (:++:) a b c
  (<*>) (a :->: b) c = appBinf (:->:) a b c
  (<*>) (a :<->: b) c = appBinf (:<->:) a b c
  (<*>) (All fs) c = All $ appListf fs c
  (<*>) (Some fs) c = Some $ appListf fs c
  (<*>) (None fs) c = None $ appListf fs c
  (<*>) (ExactlyOne fs) c = ExactlyOne $ appListf fs c
  (<*>) (AtMostOne fs) c = AtMostOne $ appListf fs c

appBinf :: Applicative f
        => (f b1 -> f b2 -> t) -> f (a -> b1) -> f (a -> b2) -> f a -> t
appBinf f x y z = (x <*> z) `f` (y <*> z)

appListf :: (Applicative f1, Functor f2) =>
                  f2 (f1 (a -> b)) -> f1 a -> f2 (f1 b)
appListf fs c = ((\f -> f <*> c) <$> fs)

infixr 6 :&&:
infixr 5 :||:
infixr 4 :++:
infixr 2 :->:
infix  1 :<->:

translateToSAT :: Formula v -> S.Formula v
translateToSAT (Var v)         = S.Var v
translateToSAT Yes             = S.Yes
translateToSAT No              = S.No
translateToSAT (Not c)         = S.Not (translateToSAT c)
translateToSAT (a :&&:  b)     = (translateToSAT a) S.:&&:  (translateToSAT b)
translateToSAT (a :||:  b)     = (translateToSAT a) S.:||:  (translateToSAT b)
translateToSAT (a :++:  b)     = (translateToSAT a) S.:++:  (translateToSAT b)
translateToSAT (a :->:  b)     = (translateToSAT a) S.:->:  (translateToSAT b)
translateToSAT (a :<->: b)     = (translateToSAT a) S.:<->: (translateToSAT b)
translateToSAT (All cs)        = S.All (translateToSAT <$> cs)
translateToSAT (Some cs)       = S.Some (translateToSAT <$> cs)
translateToSAT (None cs)       = S.None (translateToSAT <$> cs)
translateToSAT (ExactlyOne cs) = S.ExactlyOne (translateToSAT <$> cs)
translateToSAT (AtMostOne cs)  = S.AtMostOne (translateToSAT <$> cs)

instance (Eq v) => Eq (Formula v) where
  a == b = (translateToSAT a) == (translateToSAT b)

instance (Ord v) => Ord (Formula v) where
  compare a b = compare (translateToSAT a) (translateToSAT b)

instance (Show v) => Show (Formula v) where
  showsPrec d a = showsPrec d (translateToSAT a)

satisfiable :: Ord v => Formula v -> Bool
satisfiable = S.satisfiable . translateToSAT

solve_all :: Ord v => Formula v -> [Map v Bool]
solve_all = S.solve_all . translateToSAT
