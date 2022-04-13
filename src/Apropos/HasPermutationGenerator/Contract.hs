module Apropos.HasPermutationGenerator.Contract (
  Contract,
  branches,
  branchIf,
  has,
  hasn't,
  hasAll,
  hasNone,
  add,
  addIf,
  addAll,
  addAllIf,
  remove,
  removeIf,
  removeAll,
  removeAllIf,
  clear,
  terminal,
  matches,
  solveContract,
  solveContractList,
  solveEdgesMap,
  runContract,
  labelContract,
  solvesContract,
  P(..),
) where

import Apropos.LogicalModel (
  solveAll,
  Formula(..),
  Enumerable(..),
  LogicalModel(logic),
                            )
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)

toInstructions :: Contract p () -> [Instruction p]
toInstructions = execWriter

add :: a -> Contract a ()
add = addAll . pure

addAll :: [a] -> Contract a ()
addAll = tell . pure . Adds

remove :: a -> Contract a ()
remove = removeAll . pure

removeAll :: [a] -> Contract a ()
removeAll = tell . pure . Removes

branches :: [Contract a ()] -> Contract a ()
branches = tell . pure . Branch . map toInstructions

matches :: Formula a -> Contract a ()
matches = tell . pure . Holds

has :: a -> Contract a ()
has = matches . Var

hasAll :: [a] -> Contract a ()
hasAll = matches . All . map Var

hasNone :: [a] -> Contract a ()
hasNone = matches . None . map Var

hasn't :: a -> Contract a ()
hasn't = matches . Not . Var

addIf :: p -> p -> Contract p ()
addIf p q = branches [has p >> add q, hasn't p]

addAllIf :: p -> [p] -> Contract p ()
addAllIf p qs = branches [has p >> addAll qs, hasn't p]

removeIf :: p -> p -> Contract p ()
removeIf p q = branches [has p >> remove q, hasn't p]

removeAllIf :: p -> [p] -> Contract p ()
removeAllIf p qs = branches [has p >> removeAll qs, hasn't p]

branchIf :: p -> Contract p () -> Contract p () -> Contract p ()
branchIf p a b = branches [has p >> a, hasn't p >> b]

clear :: Enumerable p => Contract p ()
clear = removeAll enumerated

terminal :: Contract p ()
terminal = matches No

data Instruction p
  = Adds [p]
  | Removes [p]
  | Branch [[Instruction p]]
  | Holds (Formula p)
  deriving stock (Show, Functor)

type Contract p = Writer [Instruction p]

data S a = S Int a deriving stock (Eq, Ord)

instance Show a => Show (S a) where
  show (S n a) = show a ++ "@" ++ show n

sFirst :: (Int -> Int) -> S a -> S a
sFirst f (S n a) = S (f n) a

data EdgeFormula p = EdgeFormula
  { form :: Formula (S p)
  , space :: Int
  --, antiValidity :: Formula (S p)
  , i :: Int
  , o :: Int
  }
  deriving stock (Show)

translateInstruction :: Enumerable p => Instruction p -> EdgeFormula p
translateInstruction (Adds ps) =
  EdgeFormula
    (All [Var (S 0 v) :<->: Var (S 1 v) | v <- enumerated, v `notElem` ps] :&&: All [(Var (S 0 p) :||: Not (Var (S 0 p))) :&&: Var (S 1 p) | p <- ps])
    1
    --No
    0
    1
translateInstruction (Removes ps) =
  EdgeFormula
    (All [Var (S 0 v) :<->: Var (S 1 v) | v <- enumerated, v `notElem` ps] :&&: None [(Var (S 0 p) :||: Not (Var (S 0 p))) :&&: Var (S 1 p) | p <- ps])
    1
    --No
    0
    1
translateInstruction (Holds f) = EdgeFormula (S 0 <$> f) 0 {- No -} 0 0
translateInstruction (Branch bs) =
  let pbs = translateInstructions <$> bs
      scanRes = scanl (+) 2 ((+1) . space <$> pbs)
      starts = init scanRes
      ranges = zip scanRes (subtract 1 <$> tail scanRes)
      blanks = [ None [Var (S t e) | e <- enumerated , t <- [a..b] ] | (a,b) <- ranges ]
      forms = zipWith (\start form' -> sFirst (+ start) <$> form') starts (form <$> pbs)
      --antis = zipWith (\start anti' -> sFirst (+ start) <$> anti') starts (antiValidity <$> pbs)
      cons = zipWith (\start pb -> All [(Var (S 0 a) :<->: Var (S (start + i pb) a)) :&&: (Var (S 1 a) :<->: Var (S (start + o pb) a)) | a <- enumerated]) starts pbs
      brs = zipWith (:&&:) cons forms
      --newAnti = Some $ do
      --  (a, b) <- distinctPairs pbs
      --  let astart = 3
      --      aend = 3 + space a
      --      bstart = aend + 1
      --      ai = astart + i a
      --      ao = astart + o a
      --      bi = bstart + i b
      --      bo = bstart + o b
      --      con0a = All [Var (S 0 p) :<->: Var (S ai p) | p <- enumerated]
      --      con0b = All [Var (S 0 p) :<->: Var (S bi p) | p <- enumerated]
      --      cona1 = All [Var (S ao p) :<->: Var (S 1 p) | p <- enumerated]
      --      conb2 = All [Var (S bo p) :<->: Var (S 2 p) | p <- enumerated]
      --      forma = sFirst (+ astart) <$> form a
      --      formb = sFirst (+ bstart) <$> form b
      --  pure $
      --    All
      --      [ con0a
      --      , con0b
      --      , cona1
      --      , conb2
      --      , forma
      --      , formb
      --      , Not $ All [Var (S 1 p) :<->: Var (S 2 p) | p <- enumerated]
      --      ]
   in EdgeFormula (Some brs :&&: All (zipWith (:||:) brs blanks)) (2 + last scanRes) {- (Some antis :||: newAnti) -} 0 1

translateInstructions :: Enumerable p => [Instruction p] -> EdgeFormula p
translateInstructions [] = idPartial
translateInstructions [x] = translateInstruction x
translateInstructions (x : xs) = foldl seqFormulas (translateInstruction x) $ map translateInstruction xs

idPartial :: EdgeFormula p
idPartial = EdgeFormula Yes 0 {- No -} 0 0

-- TODO minimize conection logic by just coliding values
seqFormulas :: Enumerable p => EdgeFormula p -> EdgeFormula p -> EdgeFormula p
seqFormulas l r =
  let lstart = 0
      lend = lstart + space l
      li = lstart + i l
      lo = lstart + o l
      rstart = lend + 1
      rend = rstart + space r
      ri = rstart + i r
      ro = rstart + o r
      conectlr = All [Var (S lo a) :<->: Var (S ri a) | a <- enumerated]
      lform = sFirst (+ lstart) <$> form l
      rform = sFirst (+ rstart) <$> form r
      --lanti = sFirst (+ lstart) <$> antiValidity l
      --ranti = sFirst (+ rstart) <$> antiValidity r
   in EdgeFormula (All [conectlr, lform, rform]) rend {- (lanti :||: ranti) -} li ro

solveEdgesList :: (Enumerable p) => EdgeFormula p -> [(Set p, Set p)]
solveEdgesList c =
--  case solveAll (antiValidity c) of
--    [] ->
      [ (inprops , outprops)
      | s <- solveAll (form c)
      , let inprops = Set.fromList [k | k <- enumerated, Map.lookup (S (i c) k) s == Just True]
      , let outprops = Set.fromList [k | k <- enumerated, Map.lookup (S (o c) k) s == Just True]
      , inprops /= outprops
      ]
--    s ->
--      let trues = Map.keys $ Map.filter id $ head s
--          falses = Map.keys $ Map.filter not $ head s
--       in error $
--            "\n ambiguous branches"
--              ++ "\n formula "
--              ++ show (antiValidity c)
--              ++ "\n in: true "
--              ++ show [a | S 0 a <- trues]
--              ++ " false "
--              ++ show [a | S 0 a <- falses]
--              ++ "\n out1: true "
--              ++ show [a | S 1 a <- trues]
--              ++ " false "
--              ++ show [a | S 1 a <- falses]
--              ++ "\n out2: true "
--              ++ show [a | S 2 a <- trues]
--              ++ " false "
--              ++ show [a | S 2 a <- falses]

-- TODO better message here

withLogic :: LogicalModel p => EdgeFormula p -> EdgeFormula p
withLogic e@EdgeFormula{form=f} = e{form = f :&&: (S (i e) <$> logic) :&&: (S (o e) <$> logic)}

solveEdgesMap :: (Enumerable p) => EdgeFormula p -> Map (Set p) (Set p)
solveEdgesMap = Map.fromList . solveEdgesList

--distinctPairs :: [a] -> [(a, a)]
--distinctPairs (x : xs) = ((x,) <$> xs) ++ distinctPairs xs
--distinctPairs [] = []

solveContractList :: LogicalModel p => Contract p () -> [(Set p, Set p)]
solveContractList = solveEdgesList . withLogic . translateInstructions . toInstructions

solveContract :: LogicalModel p => Contract p () -> Set (Set p, Set p)
solveContract = Set.fromList . solveContractList

solveContractMap :: LogicalModel p => Contract p () -> Map (Set p) (Set p)
solveContractMap = Map.fromList . solveContractList

runContract :: LogicalModel p => Contract p () -> Set p -> Maybe (Set p)
runContract c ps = Map.lookup ps (solveContractMap c)

labelContract :: (a -> b) -> Contract a () -> Contract b ()
labelContract f = tell . map (fmap f) . execWriter

data P = A | B | C | D
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Enumerable)

solvesContract :: LogicalModel p => Contract p () -> Set p -> Set p -> Bool
solvesContract c iprops oprops = let
  ef = withLogic . translateInstructions . toInstructions $ c
  inlogic = All [ (if p `elem` iprops then id else Not) $ Var (S (i ef) p) | p <- enumerated ]
  outlogic = All [ (if p `elem` oprops then id else Not) $ Var (S (o ef) p) | p <- enumerated ]
    in not . null $ solveAll (form ef :&&: inlogic :&&: outlogic)

