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
) where

import Apropos.LogicalModel (
  Enumerable (..),
  Formula (..),
  LogicalModel (logic),
  satisfiesFormula,
  solveAll,
 )
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

toInstructions :: Ord p => Contract p () -> [Instruction p]
toInstructions = cleanContract . execWriter

add :: Ord a => a -> Contract a ()
add = addAll . pure

addAll :: Ord a => [a] -> Contract a ()
addAll = tell . pure . Delta Set.empty . Set.fromList

remove :: Ord a => a -> Contract a ()
remove = removeAll . pure

removeAll :: Ord a => [a] -> Contract a ()
removeAll = tell . pure . (`Delta` Set.empty) . Set.fromList

branches :: Ord a => [Contract a ()] -> Contract a ()
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

addIf :: Ord p => p -> p -> Contract p ()
addIf p q = branches [has p >> add q, hasn't p]

addAllIf :: Ord p => p -> [p] -> Contract p ()
addAllIf p qs = branches [has p >> addAll qs, hasn't p]

removeIf :: Ord p => p -> p -> Contract p ()
removeIf p q = branches [has p >> remove q, hasn't p]

removeAllIf :: Ord p => p -> [p] -> Contract p ()
removeAllIf p qs = branches [has p >> removeAll qs, hasn't p]

branchIf :: Ord p => p -> Contract p () -> Contract p () -> Contract p ()
branchIf p a b = branches [has p >> a, hasn't p >> b]

clear :: Enumerable p => Contract p ()
clear = removeAll enumerated

terminal :: Contract p ()
terminal = matches No

data Instruction p
  = Delta (Set p) (Set p) -- removes then adds lists have an empty intersection
  | Branch [[Instruction p]]
  | Holds (Formula p)
  deriving stock (Show)

instrMap :: Ord b => (a -> b) -> Instruction a -> Instruction b
instrMap f (Delta rs as) = Delta (Set.map f rs) (Set.map f as)
instrMap f (Branch bs) = Branch $ map (map (instrMap f)) bs
instrMap f (Holds fo) = Holds (fmap f fo)

runContract :: LogicalModel p => Contract p () -> Set p -> Either String (Set p)
runContract c s =
  let ss = interprets (toInstructions c) s
   in case filter (satisfiesFormula logic) (Set.toList ss) of
        [] -> Left "contract has no result"
        [x] -> Right x
        xs -> Left $ "contract has multiple results " <> show xs

interprets :: LogicalModel p => [Instruction p] -> Set p -> Set (Set p)
interprets [] s = Set.singleton s
interprets (x : xs) s =
  let r = interpret x s
      rs = interprets xs <$> Set.toList r
   in foldr Set.union Set.empty rs

interpret :: forall p. LogicalModel p => Instruction p -> Set p -> Set (Set p)
interpret (Delta rs as) is = Set.singleton $ (is `Set.difference` rs) `Set.union` as
interpret (Holds f) is =
  if satisfiesFormula f is
    then Set.singleton is
    else Set.empty
interpret (Branch bs) is =
  let cs :: [Set (Set p)]
      cs = uncurry interprets <$> zip bs (repeat is)
   in foldr Set.union Set.empty cs

type Contract p = Writer [Instruction p]

data S a = S Int a deriving stock (Eq, Ord)

instance Show a => Show (S a) where
  show (S n a) = show a ++ "@" ++ show n

sFirst :: (Int -> Int) -> S a -> S a
sFirst f (S n a) = S (f n) a

data EdgeFormula p = EdgeFormula
  { form :: Formula (S p)
  , space :: Int
  , i :: Int
  , o :: Int
  }
  deriving stock (Show)

translateInstruction :: Enumerable p => Instruction p -> EdgeFormula p
translateInstruction (Delta rs as) =
  EdgeFormula
    ((All [Var (S 0 v) :<->: Var (S 1 v) | v <- enumerated, v `notElem` (rs `Set.union` as)]) :&&: All [Not $ Var (S 1 p) | p <- Set.toList rs] :&&: All [Var (S 1 p) | p <- Set.toList as])
    1
    0
    1
translateInstruction (Holds f) = EdgeFormula (S 0 <$> f) 0 0 0
translateInstruction (Branch bs) =
  let pbs = translateInstructions <$> bs
      scanRes = scanl (+) 2 ((+ 1) . space <$> pbs)
      starts = init scanRes
      ranges = zip scanRes (subtract 1 <$> tail scanRes)
      blanks = [None [Var (S t e) | e <- enumerated, t <- [a .. b]] | (a, b) <- ranges]
      forms = zipWith (\start form' -> sFirst (+ start) <$> form') starts (form <$> pbs)
      cons = zipWith (\start pb -> All [(Var (S 0 a) :<->: Var (S (start + i pb) a)) :&&: (Var (S 1 a) :<->: Var (S (start + o pb) a)) | a <- enumerated]) starts pbs
      brs = zipWith (:&&:) cons forms
   in EdgeFormula (Some brs :&&: All (zipWith (:||:) brs blanks)) (2 + last scanRes) 0 1

translateInstructions :: Enumerable p => [Instruction p] -> EdgeFormula p
translateInstructions [] = idPartial
translateInstructions [x] = translateInstruction x
translateInstructions (x : xs) = foldl seqFormulas (translateInstruction x) $ map translateInstruction xs

idPartial :: EdgeFormula p
idPartial = EdgeFormula Yes 0 0 0

-- TODO this can be changed to add one less timestamp
-- which may be a performance imrpovement
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
   in EdgeFormula (All [conectlr, lform, rform]) rend li ro

solveEdgesList :: (Enumerable p) => EdgeFormula p -> [(Set p, Set p)]
solveEdgesList c =
  [ (inprops, outprops)
  | s <- solveAll (form c)
  , let inprops = Set.fromList [k | k <- enumerated, Map.lookup (S (i c) k) s == Just True]
  , let outprops = Set.fromList [k | k <- enumerated, Map.lookup (S (o c) k) s == Just True]
  , inprops /= outprops
  ]

withLogic :: LogicalModel p => EdgeFormula p -> EdgeFormula p
withLogic e@EdgeFormula {form = f} =
  e
    { form =
        f :&&: (S (i e) <$> logic) :&&: (S (o e) <$> logic)
          --these last two lines just ensure input and output vars are all mentioned
          :&&: All [Var (S (o e) p) :||: Not (Var (S (o e) p)) | p <- enumerated]
          :&&: All [Var (S (i e) p) :||: Not (Var (S (i e) p)) | p <- enumerated]
    }


solveEdgesMap :: (Enumerable p) => EdgeFormula p -> Map (Set p) (Set p)
solveEdgesMap = Map.fromList . solveEdgesList

solveContractList :: LogicalModel p => Contract p () -> [(Set p, Set p)]
solveContractList = solveEdgesList . withLogic . translateInstructions . toInstructions

solveContract :: LogicalModel p => Contract p () -> Set (Set p, Set p)
solveContract = Set.fromList . solveContractList

labelContract :: Ord b => (a -> b) -> Contract a () -> Contract b ()
labelContract f = tell . map (instrMap f) . execWriter

cleanContract :: Ord p => [Instruction p] -> [Instruction p]
cleanContract (Delta r1 a1 : Delta r2 a2 : c) = cleanContract $ Delta ((r1 `Set.difference` a2) `Set.union` r2) ((a1 `Set.difference` r2) `Set.union` a2) : c
cleanContract (Branch bs : c) = Branch (cleanContract <$> bs) : cleanContract c
cleanContract (c : cs) = c : cleanContract cs
cleanContract [] = []
