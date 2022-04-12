module Apropos.SetFunctionFormula (
  findEdges,
  findEdgesSeq,
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
    sols = solveAll $ (S 0 <$> matches) :&&: setFunctionToFormula 0 expr
    tras = solutionToSetTranslation (0,1) <$> sols
    changes = filter (uncurry (/=)) tras


findEdgesSeq :: Enumerable a => [(Formula a, SetFunctionLanguage a ())] -> Set (Set a, Set a)
findEdgesSeq [] = Set.empty
findEdgesSeq exprs = Set.fromList changes
  where
    sols = solveAll $ All ((\(idx, (f,e)) -> (S idx <$> f) :&&: setFunctionToFormula idx e) <$> zip [0..] exprs)
    tras = solutionToSetTranslation (0, length exprs) <$> sols
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

data S a = S Int a deriving stock (Eq, Ord)

setFunctionToFormula :: Enumerable a => Int -> SetFunctionLanguage a () -> Formula (S a)
setFunctionToFormula i = mapReprToFormula . setFunctionToRepr i

solutionToSetTranslation :: Enumerable a => (Int,Int) -> Map (S a) Bool -> (Set a, Set a)
solutionToSetTranslation (iidx,oidx) sol = (i, o)
  where
    i = Set.fromList [x | x <- enumerated, fromMaybe (error "undefined variable") (Map.lookup (S iidx x) sol)]
    o = Set.fromList [x | x <- enumerated, fromMaybe (error "undefined variable") (Map.lookup (S oidx x) sol)]

data SetFunctionRepr a
  = ImplicationMap Int (Map (Formula a) (Formula a))
  | BranchRepr [SetFunctionRepr a]
  | HoldsRepr Int (Formula a) (SetFunctionRepr a)

idSetFunctionRepr :: Enumerable a => Int -> SetFunctionRepr a
idSetFunctionRepr i =
  ImplicationMap i $
    Map.fromList $
      join
        [ [ (Var x, Var x)
          , (Not (Var x), Not (Var x))
          ]
        | x <- enumerated
        ]

setFunctionToRepr :: Enumerable a => Int -> SetFunctionLanguage a () -> SetFunctionRepr a
setFunctionToRepr i sfl = execState (evalSetFunctionOnRepr i sfl) (idSetFunctionRepr i)

mapReprToFormula :: SetFunctionRepr a -> Formula (S a)
mapReprToFormula (ImplicationMap iidx mrep) =
  All
    [ (S iidx <$> i) :->: (S (iidx + 1) <$> o)
    | (i, o) <- Map.toList mrep
    ]
mapReprToFormula (BranchRepr reprs) = Some (mapReprToFormula <$> reprs)
mapReprToFormula (HoldsRepr i f repr) = (S i <$> f) :&&: mapReprToFormula repr

evalSetFunctionOnRepr :: Enumerable a => Int -> SetFunctionLanguage a () -> State (SetFunctionRepr a) ()
evalSetFunctionOnRepr iidx (Free (Holds e next)) = do
  s <- get
  put $ HoldsRepr iidx e s
  evalSetFunctionOnRepr iidx next
evalSetFunctionOnRepr iidx (Free (Branch bs next)) = do
  s <- get
  let ns = (\b -> execState (evalSetFunctionOnRepr iidx b) s) <$> bs
  put $ BranchRepr ns
  evalSetFunctionOnRepr iidx next
evalSetFunctionOnRepr iidx (Free (Adds a next)) = do
  s <- get
  case s of
    ImplicationMap i m -> do
      let m' = Map.insert (Not (Var a)) (Var a) (Map.insert (Var a) (Var a) m)
      put $ ImplicationMap i m'
    BranchRepr bs -> do
      let ns = execState (evalSetFunctionOnRepr iidx (Free (Adds a next))) <$> bs
      put $ BranchRepr ns
    HoldsRepr i f rep -> do
      put $ HoldsRepr i f $ execState (evalSetFunctionOnRepr iidx (Free (Adds a next))) rep
  evalSetFunctionOnRepr iidx next
evalSetFunctionOnRepr iidx (Free (Removes a next)) = do
  s <- get
  case s of
    ImplicationMap i m -> do
      let m' = Map.insert (Not (Var a)) (Not (Var a)) (Map.insert (Var a) (Not (Var a)) m)
      put $ ImplicationMap i m'
    BranchRepr bs -> do
      let ns = execState (evalSetFunctionOnRepr iidx (Free (Removes a next))) <$> bs
      put $ BranchRepr ns
    HoldsRepr i f rep -> do
      put $ HoldsRepr i f $ execState (evalSetFunctionOnRepr iidx (Free (Removes a next))) rep
  evalSetFunctionOnRepr iidx next
evalSetFunctionOnRepr _ (Pure next) = pure next
