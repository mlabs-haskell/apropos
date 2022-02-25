module Apropos.HasPermutationGenerator.Contract (
  Contract,
  PropertyProjection(..),
  projection,
  runContract,
  readContractInput,
  readContractEdgeName,
  readContractOutput,
  writeContractOutput,
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
  output,
) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Free
import Data.Maybe (catMaybes,isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  renderStyle,
  style,
  ($+$),
 )
import Text.Show.Pretty (ppDoc)

data FreeContract p next =
    ReadContractInput (Set p -> next)
  | ReadContractEdgeName (String -> next)
  | ReadContractOutput (Set p -> next)
  | WriteContractOutput (Set p) next
  | ContractError String next
  | Terminal next

instance Functor (FreeContract p) where
  fmap f (ReadContractInput r) = ReadContractInput (f . r)
  fmap f (ReadContractEdgeName r) = ReadContractEdgeName (f . r)
  fmap f (ReadContractOutput r) = ReadContractOutput (f . r)
  fmap f (WriteContractOutput r next) = WriteContractOutput r (f next)
  fmap f (ContractError err next) = ContractError err (f next)
  fmap f (Terminal next) = Terminal (f next)

data PropertyProjection b a = PropertyProjection {
    projectionName :: String
  , projectionFunction :: b -> Maybe a
  , injectionFunction :: a -> b
  }

projection :: Ord a => Ord b => PropertyProjection b a -> Contract a () -> Contract b ()
projection p c = do
  i <- readContractOutput
  n <- readContractEdgeName
  let subm = extractSubmodel (projectionFunction p) i
      subx = exciseSubmodel (projectionFunction p) i
      res = runContract c ((projectionName p) <> n) subm
  case res of
    Left err -> contractError err
    Right Nothing -> terminal
    Right (Just upd) ->
      output (subx `Set.union` (Set.map (injectionFunction p) upd))
  where
    extractSubmodel :: Ord p => (q -> Maybe p) -> Set q -> Set p
    extractSubmodel f q = Set.fromList $ catMaybes (f <$> Set.toList q)
    exciseSubmodel :: (q -> Maybe p) -> Set q -> Set q
    exciseSubmodel f q = Set.filter (isNothing . f) q

type Contract p = Free (FreeContract p)

readContractInput :: Contract p (Set p)
readContractInput = liftF (ReadContractInput id)

readContractEdgeName :: Contract p String
readContractEdgeName = liftF (ReadContractEdgeName id)

readContractOutput :: Contract p (Set p)
readContractOutput = liftF (ReadContractOutput id)

writeContractOutput :: Set p -> Contract p ()
writeContractOutput s = liftF (WriteContractOutput s ())

contractError :: String -> Contract p ()
contractError s = liftF (ContractError s ())

terminal :: Contract p ()
terminal = liftF (Terminal ())

type ContractRun p a = MaybeT (ReaderT (String, Set p) (StateT (Set p) (Either String))) a

interpret :: Contract p () -> ContractRun p ()
interpret (Free (ReadContractInput next))     = (snd <$> lift ask) >>= interpret . next
interpret (Free (ReadContractEdgeName next))  = (fst <$> lift ask) >>= interpret . next
interpret (Free (ReadContractOutput next))    = (lift $ lift get)  >>= interpret . next
interpret (Free (WriteContractOutput s next)) = (lift $ lift $ put s) >> interpret next
interpret (Free (ContractError err next))     = (lift $ lift $ lift $ Left err) >> interpret next
interpret (Free (Terminal next))              = fail "terminal" >> interpret next
interpret (Pure a)                            = pure a

runContract :: Contract p () -> String -> Set p -> Either String (Maybe (Set p))
runContract = runContract' . interpret
  where
    runContract' :: ContractRun p () -> String -> Set p -> Either String (Maybe (Set p))
    runContract' c nm s = do
      (b, s') <- runStateT (runReaderT (runMaybeT c) (nm, s)) s
      case b of
        Just () -> pure $ Just s'
        Nothing -> pure Nothing

runContractInternal :: Contract p () -> String -> Set p -> Set p -> Contract p (Maybe (Set p))
runContractInternal = runContractInternal' . interpret
  where
    runContractInternal' :: ContractRun p () -> String -> Set p -> Set p -> Contract p (Maybe (Set p))
    runContractInternal' c nm s i = do
      case runStateT (runReaderT (runMaybeT c) (nm, s)) i of
        Left err -> contractError err >> pure Nothing
        Right (b, s') -> case b of
                           Just () -> pure $ Just s'
                           Nothing -> pure Nothing

--e.g. has ThisThing >> add ThatThing
has :: Eq p => p -> Contract p ()
has p = do
  s <- readContractInput
  if p `elem` s
    then pure ()
    else terminal

--e.g. hasAll [ThisThing,ThatThing] >> add TheOtherThing
hasAll :: Eq p => [p] -> Contract p ()
hasAll = mapM_ has

--e.g. hasn't ThisThing >> add ThatThing
hasn't :: Eq p => p -> Contract p ()
hasn't p = do
  s <- readContractInput
  if p `elem` s
    then terminal
    else pure ()

--e.g. hasNone [ThisThing,ThatThing] >> add TheOtherThing
hasNone :: Eq p => [p] -> Contract p ()
hasNone = mapM_ hasn't

add :: Ord p => p -> Contract p ()
add p = readContractOutput >>= writeContractOutput . Set.insert p

remove :: Ord p => p -> Contract p ()
remove p = readContractOutput >>= writeContractOutput . Set.delete p

addAll :: Ord p => [p] -> Contract p ()
addAll = mapM_ add

removeAll :: Ord p => [p] -> Contract p ()
removeAll = mapM_ remove

clear :: Contract p ()
clear = writeContractOutput Set.empty

output :: Set p -> Contract p ()
output = writeContractOutput

addIf :: (Ord p, Show p) => p -> p -> Contract p ()
addIf p q = branches [has p >> add q, hasn't p]

addAllIf :: (Ord p, Show p) => p -> [p] -> Contract p ()
addAllIf p qs = branches [has p >> addAll qs, hasn't p]

removeIf :: (Ord p, Show p) => p -> p -> Contract p ()
removeIf p q = branches [has p >> remove q, hasn't p]

removeAllIf :: (Ord p, Show p) => p -> [p] -> Contract p ()
removeAllIf p qs = branches [has p >> removeAll qs, hasn't p]

branchIf :: (Eq p, Show p) => p -> Contract p () -> Contract p () -> Contract p ()
branchIf p a b = branches [has p >> a, hasn't p >> b]

branches :: (Eq p, Show p) => [Contract p ()] -> Contract p ()
branches cs = do
  i <- readContractInput
  e <- readContractEdgeName
  o <- readContractOutput
  rs <- mapM (\c -> runContractInternal c e i o) cs
  case catMaybes rs of
    [] -> terminal
    [ao] -> writeContractOutput ao
    (ao : rest) ->
      if all (== ao) rest
        then writeContractOutput ao
        else contractError $ renderStyle ourStyle $
                    (fromString $ "PermutationEdge " <> e <> " has non-deterministic type")
                      $+$ hang "error:" 4 "branches succeeded with different results in each branch."
                      $+$ hang "results:" 4 (ppDoc (ao : rest))

ourStyle :: Style
ourStyle = style {lineLength = 80}
