{-# LANGUAGE TypeFamilies #-}

module Apropos.LogicalModel.HasLogicalModel (
  HasLogicalModel (..),
  Prop (Prop, unProp),
  var,
) where

import Apropos.Logic as L
import Apropos.LogicalModel as LM

import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Set qualified as Set

class (LogicalModel p) => HasLogicalModel p m | p -> m where
  satisfiesProperty :: p -> m -> Bool

  satisfiesAny :: [p] -> m -> Bool
  satisfiesAny ps m = or (flip satisfiesProperty m <$> ps)
  satisfiesAll :: [p] -> m -> Bool
  satisfiesAll ps m = and (flip satisfiesProperty m <$> ps)
  properties :: m -> Set p
  default properties :: (Enumerable p) => m -> Set p
  properties x = Set.fromList $ filter (`satisfiesProperty` x) enumerated

newtype Prop p = Prop {unProp :: p}
  deriving stock (Eq, Ord, Show)
  deriving (Enumerable, Hashable) via p

instance (Enumerable p, HasLogicalModel p m) => Strategy (Prop p) m where
  type Properties (Prop p) = Set p

  logic = Prop <$> (LM.logic :&&: allPresentInFormula)
    where
      allPresentInFormula :: Formula p
      allPresentInFormula = All (mention <$> (enumerated :: [p]))
      mention :: p -> Formula p
      mention p = Var p :||: Not (Var p)

  universe = enumerated

  toProperties = properties

  propertiesToVariables = Set.map Prop

  variablesToProperties = Set.map unProp

var :: v -> Formula (Prop v)
var = Var . Prop
