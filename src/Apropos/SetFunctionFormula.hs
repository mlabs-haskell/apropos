module Apropos.SetFunctionFormula (
  findEdges,
  adds,
  removes,
  branches,
  holds,
  has,
  SetFunctionLanguage,
) where

import Apropos.LogicalModel.Enumerable
import Apropos.LogicalModel.Formula
import Control.Monad (join)
import Control.Monad.Free
import Control.Monad.State (State, execState, get, put)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

findEdges :: Enumerable a => Formula a -> SetFunctionLanguage a () -> Set (Set a, Set a)
findEdges matches expr = Set.fromList changes
  where
    sols = solveAll $ (I <$> matches) :&&: setFunctionToFormula expr
    tras = solutionToSetTranslation <$> sols
    changes = filter (uncurry (/=)) tras

adds :: a -> SetFunctionLanguage a ()
adds a = liftF (Adds a ())

removes :: a -> SetFunctionLanguage a ()
removes a = liftF (Removes a ())

branches :: [SetFunctionLanguage a ()] -> SetFunctionLanguage a ()
branches as = liftF (Branch as ())

holds :: Formula a -> SetFunctionLanguage a ()
holds f = liftF (Holds f ())

has :: a -> SetFunctionLanguage a ()
has = holds . Var

data FreeSetFunctionLanguage a next
  = Adds a next
  | Removes a next
  | Branch [SetFunctionLanguage a ()] next
  | Holds (Formula a) next

instance Functor (FreeSetFunctionLanguage a) where
  fmap f (Adds a next) = Adds a (f next)
  fmap f (Removes a next) = Removes a (f next)
  fmap f (Branch as next) = Branch as (f next)
  fmap f (Holds e next) = Holds e (f next)

type SetFunctionLanguage a = Free (FreeSetFunctionLanguage a)

data S a = I a | O a deriving stock (Eq, Ord)

setFunctionToFormula :: Enumerable a => SetFunctionLanguage a () -> Formula (S a)
setFunctionToFormula = mapReprToFormula . setFunctionToRepr

solutionToSetTranslation :: Enumerable a => Map (S a) Bool -> (Set a, Set a)
solutionToSetTranslation sol = (i, o)
  where
    i = Set.fromList [x | x <- enumerated, fromMaybe (error "undefined variable") (Map.lookup (I x) sol)]
    o = Set.fromList [x | x <- enumerated, fromMaybe (error "undefined variable") (Map.lookup (O x) sol)]

data SetFunctionRepr a
  = ImplicationMap (Map (Formula a) (Formula a))
  | BranchRepr [SetFunctionRepr a]
  | HoldsRepr (Formula a) (SetFunctionRepr a)

idSetFunctionRepr :: Enumerable a => SetFunctionRepr a
idSetFunctionRepr =
  ImplicationMap $
    Map.fromList $
      join
        [ [ (Var x, Var x)
          , (Not (Var x), Not (Var x))
          ]
        | x <- enumerated
        ]

setFunctionToRepr :: Enumerable a => SetFunctionLanguage a () -> SetFunctionRepr a
setFunctionToRepr sfl = execState (evalSetFunctionOnRepr sfl) idSetFunctionRepr

mapReprToFormula :: SetFunctionRepr a -> Formula (S a)
mapReprToFormula (ImplicationMap mrep) =
  All
    [ (I <$> i) :->: (O <$> o)
    | (i, o) <- Map.toList mrep
    ]
mapReprToFormula (BranchRepr reprs) = Some (mapReprToFormula <$> reprs)
mapReprToFormula (HoldsRepr f repr) = (I <$> f) :&&: mapReprToFormula repr

evalSetFunctionOnRepr :: Enumerable a => SetFunctionLanguage a () -> State (SetFunctionRepr a) ()
evalSetFunctionOnRepr (Free (Holds e next)) = do
  s <- get
  put $ HoldsRepr e s
  evalSetFunctionOnRepr next
evalSetFunctionOnRepr (Free (Branch bs next)) = do
  s <- get
  let ns = (\b -> execState (evalSetFunctionOnRepr b) s) <$> bs
  put $ BranchRepr ns
  evalSetFunctionOnRepr next
evalSetFunctionOnRepr (Free (Adds a next)) = do
  s <- get
  case s of
    ImplicationMap m -> do
      let m' = Map.insert (Not (Var a)) (Var a) (Map.insert (Var a) (Var a) m)
      put $ ImplicationMap m'
    BranchRepr bs -> do
      let ns = execState (evalSetFunctionOnRepr (Free (Adds a next))) <$> bs
      put $ BranchRepr ns
    HoldsRepr f rep -> do
      put $ HoldsRepr f $ execState (evalSetFunctionOnRepr (Free (Adds a next))) rep
  evalSetFunctionOnRepr next
evalSetFunctionOnRepr (Free (Removes a next)) = do
  s <- get
  case s of
    ImplicationMap m -> do
      let m' = Map.insert (Not (Var a)) (Not (Var a)) (Map.insert (Var a) (Not (Var a)) m)
      put $ ImplicationMap m'
    BranchRepr bs -> do
      let ns = execState (evalSetFunctionOnRepr (Free (Removes a next))) <$> bs
      put $ BranchRepr ns
    HoldsRepr f rep -> do
      put $ HoldsRepr f $ execState (evalSetFunctionOnRepr (Free (Removes a next))) rep
  evalSetFunctionOnRepr next
evalSetFunctionOnRepr (Pure next) = pure next
