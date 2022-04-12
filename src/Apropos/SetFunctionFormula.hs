module Apropos.SetFunctionFormula (
    findEdges
  , adds
  , removes
  , SetFunctionLanguage
  ) where
import Apropos.LogicalModel.Formula
import Apropos.LogicalModel.Enumerable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Control.Monad.Free
import Control.Monad.State (State, execState, get, put)

findEdges :: Enumerable a => SetFunctionLanguage a () -> Set (Set a, Set a)
findEdges expr = Set.fromList changes
  where sols = solveAll $ setFunctionToFormula expr
        tras = solutionToSetTranslation <$> sols
        changes = filter (uncurry (/=)) tras

adds :: a -> SetFunctionLanguage a ()
adds a = liftF (Adds a ())

removes :: a -> SetFunctionLanguage a ()
removes a = liftF (Removes a ())

data FreeSetFunctionLanguage a next =
      Adds a next
    | Removes a next

instance Functor (FreeSetFunctionLanguage a) where
  fmap f (Adds a next) = Adds a (f next)
  fmap f (Removes a next) = Removes a (f next)

type SetFunctionLanguage a = Free (FreeSetFunctionLanguage a)

data S a = I a | O a deriving stock (Eq, Ord)

setFunctionToFormula :: Enumerable a => SetFunctionLanguage a () -> Formula (S a)
setFunctionToFormula = mapReprToFormula . setFunctionToMapRepr

solutionToSetTranslation :: Enumerable a => Map (S a) Bool -> (Set a, Set a)
solutionToSetTranslation sol = (i,o)
  where i = Set.fromList [ x | x <- enumerated, fromMaybe (error "undefined variable") (Map.lookup (I x) sol) ]
        o = Set.fromList [ x | x <- enumerated, fromMaybe (error "undefined variable") (Map.lookup (O x) sol) ]

idSetFunctionMapRepr :: Enumerable a => Map (Formula a) (Formula a)
idSetFunctionMapRepr =
  Map.fromList $ join [ [ (Var x, Var x)
                        , (Not (Var x), Not (Var x))
                        ]
                      | x <- enumerated
                      ]

setFunctionToMapRepr :: Enumerable a => SetFunctionLanguage a () -> Map (Formula a) (Formula a)
setFunctionToMapRepr sfl = execState (evalSetFunctionOnMapRepr sfl) idSetFunctionMapRepr

mapReprToFormula :: Map (Formula a) (Formula a) -> Formula (S a)
mapReprToFormula mrep =
  All [ (I <$> i) :->: (O <$> o)
      | (i,o) <- Map.toList mrep
      ]

evalSetFunctionOnMapRepr :: Enumerable a => SetFunctionLanguage a () -> State (Map (Formula a) (Formula a)) ()
evalSetFunctionOnMapRepr (Free (Adds a next)) = do
  m <- get
  let m' = Map.insert (Not (Var a)) (Var a) (Map.insert (Var a) (Var a) m)
  put m'
  evalSetFunctionOnMapRepr next
evalSetFunctionOnMapRepr (Free (Removes a next)) = do
  m <- get
  let m' = Map.insert (Not (Var a)) (Not (Var a)) (Map.insert (Var a) (Not (Var a)) m)
  put m'
  evalSetFunctionOnMapRepr next
evalSetFunctionOnMapRepr (Pure next) = pure next


