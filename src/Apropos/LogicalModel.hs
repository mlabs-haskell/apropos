module Apropos.LogicalModel (
  LogicalModel (..),
  Enumerable (..),
  enumerateScenariosWhere,
  satisfiesFormula,
  enumerateSolutions,
  scenarioMap,
  module Apropos.LogicalModel.Formula,
  module Apropos.LogicalModel.Enumerable,
) where

import Apropos.LogicalModel.Enumerable
import Apropos.LogicalModel.Formula
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

class (Eq p, Ord p, Show p) => LogicalModel p where
  logic :: Formula p

  scenarios :: [Set p]

  default scenarios :: (Enumerable p) => [Set p]
  scenarios = enumerateScenariosWhere (logic :: Formula p)

  satisfiedBy :: [p]
  satisfiedBy = Set.toList $
    case scenarios of
      [] -> error "no solutions found for model logic"
      (sol : _) -> sol

scenarioMap :: LogicalModel p => Map Int (Set p)
scenarioMap = Map.fromList $ zip [0 ..] scenarios

enumerateScenariosWhere :: forall p. (Enumerable p, LogicalModel p) => Formula p -> [Set p]
enumerateScenariosWhere holds = enumerateSolutions $ logic :&&: holds :&&: allPresentInFormula
  where
    allPresentInFormula :: Formula p
    allPresentInFormula = All (mention <$> (enumerated :: [p]))
    mention :: p -> Formula p
    mention p = Var p :||: Not (Var p)

enumerateSolutions :: LogicalModel p => Formula p -> [Set p]
enumerateSolutions f = Map.keysSet . Map.filter id <$> solveAll f

satisfiesFormula :: forall p. (Enumerable p) => Formula p -> Set p -> Bool
satisfiesFormula f s = satisfiable $ f :&&: All (Var <$> set) :&&: None (Var <$> unset)
  where
    set :: [p]
    set = Set.toList s
    unset :: [p]
    unset = filter (`notElem` s) (enumerated :: [p])
