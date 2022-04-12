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
  solveEdgesMap,
  runContract,
  labelContract,
) where

import Apropos.LogicalModel.Enumerable (Enumerable (..))
import Apropos.LogicalModel.Formula (Formula (..), solveAll)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

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
  , antiValidity :: Formula (S p)
  , i :: Int
  , o :: Int
  }
  deriving stock (Show)

translateInstruction :: Enumerable p => Instruction p -> EdgeFormula p
translateInstruction (Adds ps) =
  EdgeFormula
    (All [Var (S 0 v) :<->: Var (S 1 v) | v <- enumerated, v `notElem` ps] :&&: All [(Var (S 0 p) :||: Not (Var (S 0 p))) :&&: Var (S 1 p) | p <- ps])
    1
    No
    0
    1
translateInstruction (Removes ps) =
  EdgeFormula
    (All [Var (S 0 v) :<->: Var (S 1 v) | v <- enumerated, v `notElem` ps] :&&: None [(Var (S 0 p) :||: Not (Var (S 0 p))) :&&: Var (S 1 p) | p <- ps])
    1
    No
    0
    1
translateInstruction (Holds f) = EdgeFormula (S 0 <$> f) 0 No 0 0
translateInstruction (Branch bs) =
  let pbs = translateInstructions <$> bs
      scanRes = scanl (+) 2 (space <$> pbs)
      starts = init scanRes
      forms = zipWith (\start form' -> sFirst (+ start) <$> form') starts (form <$> pbs)
      antis = zipWith (\start anti' -> sFirst (+ start) <$> anti') starts (antiValidity <$> pbs)
      cons = zipWith (\start pb -> All [(Var (S 0 a) :<->: Var (S (start + i pb) a)) :&&: (Var (S 1 a) :<->: Var (S (start + o pb) a)) | a <- enumerated]) starts pbs
      newAnti = Some $ do
        (a, b) <- distinctPairs pbs
        let astart = 3
            aend = 3 + space a
            bstart = aend + 1
            ai = astart + i a
            ao = astart + o a
            bi = bstart + i b
            bo = bstart + o b
            con0a = All [Var (S 0 p) :<->: Var (S ai p) | p <- enumerated]
            con0b = All [Var (S 0 p) :<->: Var (S bi p) | p <- enumerated]
            cona1 = All [Var (S ao p) :<->: Var (S 1 p) | p <- enumerated]
            conb2 = All [Var (S bo p) :<->: Var (S 2 p) | p <- enumerated]
            forma = sFirst (+ astart) <$> form a
            formb = sFirst (+ bstart) <$> form b
        pure $
          All
            [ con0a
            , con0b
            , cona1
            , conb2
            , forma
            , formb
            , Not $ All [Var (S 1 p) :<->: Var (S 2 p) | p <- enumerated]
            ]
   in EdgeFormula (Some $ zipWith (:&&:) cons forms) (2 + last scanRes) (Some antis :||: newAnti) 0 1

translateInstructions :: Enumerable p => [Instruction p] -> EdgeFormula p
translateInstructions [] = idPartial
translateInstructions [x] = translateInstruction x
translateInstructions (x : xs) = foldl seqFormulas (translateInstruction x) $ map translateInstruction xs

idPartial :: EdgeFormula p
idPartial = EdgeFormula Yes 0 No 0 0

-- TODO minimize conection logic by just coliding values
seqFormulas :: Enumerable p => EdgeFormula p -> EdgeFormula p -> EdgeFormula p
seqFormulas l r =
  let lstart = 2
      lend = lstart + space l
      li = lstart + i l
      lo = lstart + o l
      rstart = lend + 1
      rend = rstart + space r
      ri = rstart + i r
      ro = rstart + o r
      conectlr = All [Var (S lo a) :<->: Var (S ri a) | a <- enumerated]
      conect0l = All [Var (S 0 a) :<->: Var (S li a) | a <- enumerated]
      conectr1 = All [Var (S ro a) :<->: Var (S 1 a) | a <- enumerated]
      lform = sFirst (+ lstart) <$> form l
      rform = sFirst (+ rstart) <$> form r
      lanti = sFirst (+ lstart) <$> antiValidity l
      ranti = sFirst (+ rstart) <$> antiValidity r
   in EdgeFormula (All [conect0l, conectlr, conectr1, lform, rform]) rend (lanti :||: ranti) 0 1

solveEdgesList :: (Show p, Enumerable p) => EdgeFormula p -> [(Set p, Set p)]
solveEdgesList c =
  case solveAll (antiValidity c) of
    [] ->
      [ ( Set.fromList [k | k <- enumerated, Map.lookup (S (i c) k) s == Just True]
        , Set.fromList [k | k <- enumerated, Map.lookup (S (o c) k) s == Just True]
        )
      | s <- solveAll (form c)
      ]
    s ->
      let trues = Map.keys $ Map.filter id $ head s
          falses = Map.keys $ Map.filter not $ head s
       in error $
            "\n ambiguous branches"
              ++ "\n formula "
              ++ show (antiValidity c)
              ++ "\n in: true "
              ++ show [a | S 0 a <- trues]
              ++ " false "
              ++ show [a | S 0 a <- falses]
              ++ "\n out1: true "
              ++ show [a | S 1 a <- trues]
              ++ " false "
              ++ show [a | S 1 a <- falses]
              ++ "\n out2: true "
              ++ show [a | S 2 a <- trues]
              ++ " false "
              ++ show [a | S 2 a <- falses]

-- TODO better message here

solveEdges :: (Show p, Enumerable p) => EdgeFormula p -> Set (Set p, Set p)
solveEdges = Set.fromList . solveEdgesList

solveEdgesMap :: (Show p, Enumerable p) => EdgeFormula p -> Map (Set p) (Set p)
solveEdgesMap = Map.fromList . solveEdgesList

distinctPairs :: [a] -> [(a, a)]
distinctPairs (x : xs) = ((x,) <$> xs) ++ distinctPairs xs
distinctPairs [] = []

solveContract :: (Show p, Enumerable p) => Contract p () -> Set (Set p, Set p)
solveContract = solveEdges . translateInstructions . toInstructions

solveContractMap :: (Show p, Enumerable p) => Contract p () -> Map (Set p) (Set p)
solveContractMap = solveEdgesMap . translateInstructions . toInstructions

runContract :: (Show p, Enumerable p) => Contract p () -> Set p -> Maybe (Set p)
runContract c ps = Map.lookup ps (solveContractMap c)

labelContract :: (a -> b) -> Contract a () -> Contract b ()
labelContract f = tell . map (fmap f) . execWriter
