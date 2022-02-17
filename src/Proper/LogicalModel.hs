module Proper.LogicalModel
  ( LogicalModel(..)
  , enumerateScenariosWhere
  , satisfiesFormula
  , enumerateSolutions
  ) where
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import SAT.MiniSat ( Formula (..), solve_all, satisfiable)

class (Enum p, Eq p, Ord p, Bounded p, Show p) => LogicalModel p where
  logic :: Formula p

enumerateScenariosWhere :: forall p . LogicalModel p => Formula p -> [Set p]
enumerateScenariosWhere holds = enumerateSolutions $ logic :&&: holds :&&: allPresentInFormula
  where
    allPresentInFormula :: Formula p
    allPresentInFormula = All (mention <$> ([minBound .. maxBound] :: [p]))
    mention :: p -> Formula p
    mention p = Var p :||: Not (Var p)

enumerateSolutions :: LogicalModel p => Formula p -> [Set p]
enumerateSolutions f = fromSolution <$> solve_all f
  where
    fromSolution :: LogicalModel p => Map.Map p Bool -> Set p
    fromSolution m = Set.fromList $ filter isInSet [minBound .. maxBound]
      where
        isInSet k = Just True == Map.lookup k m

satisfiesFormula :: forall p . LogicalModel p => Formula p -> Set p -> Bool
satisfiesFormula f s = satisfiable $ f :&&: All (Var <$> set) :&&: None (Var <$> unset)
  where
    set :: [p]
    set = Set.toList s
    unset :: [p]
    unset = filter (`notElem` s) ([minBound .. maxBound] :: [p])

