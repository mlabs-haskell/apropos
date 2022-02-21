module Brutus.HasPermutationGenerator.Contract
  ( Contract,
    runContract,
    readContractInput,
    readContractEdgeName,
    readContractOutput,
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
  )where
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT,runMaybeT)
import Control.Monad.Reader (ReaderT,ask,runReaderT)
import Control.Monad.State (StateT,get,put,modify,runStateT)
import Text.Show.Pretty (ppDoc)
import Text.PrettyPrint (
  Style (lineLength),
  hang,
  renderStyle,
  style,
  ($+$),
 )
import Data.String (fromString)



type Contract p a = MaybeT (ReaderT (String,Set p) (StateT (Set p) (Either String))) a

runContract :: Contract p () -> String -> Set p -> Either String (Maybe (Set p))
runContract c nm s = do
  (b,s') <- runStateT (runReaderT (runMaybeT c) (nm,s)) s
  case b of
    Just () -> pure $ Just s'
    Nothing -> pure Nothing

runContractInternal :: Contract p () -> String -> Set p -> Set p -> Either String (Maybe (Set p))
runContractInternal c nm s i = do
  (b,s') <- runStateT (runReaderT (runMaybeT c) (nm,s)) i
  case b of
    Just () -> pure $ Just s'
    Nothing -> pure Nothing

readContractInput :: Contract p (Set p)
readContractInput = snd <$> lift ask

readContractEdgeName :: Contract p String
readContractEdgeName = fst <$> lift ask

readContractOutput :: Contract p (Set p)
readContractOutput = lift $ lift get

setContractOutput :: Set p -> Contract p ()
setContractOutput = lift . lift . put

--e.g. has ThisThing >> add ThatThing
has :: Eq p => p -> Contract p ()
has p = do
  s <- readContractInput
  if p `elem` s
     then pure ()
     else fail "Nothing"

--e.g. hasAll [ThisThing,ThatThing] >> add TheOtherThing
hasAll :: Eq p => [p] -> Contract p ()
hasAll = mapM_ has

--e.g. hasn't ThisThing >> add ThatThing
hasn't :: Eq p => p -> Contract p ()
hasn't p = do
  s <- readContractInput
  if p `elem` s
     then fail "Nothing"
     else pure ()

--e.g. hasNone [ThisThing,ThatThing] >> add TheOtherThing
hasNone :: Eq p => [p] -> Contract p ()
hasNone = mapM_ hasn't


add :: Ord p => p -> Contract p ()
add p = lift $ lift $ modify (Set.insert p)


remove :: Ord p => p -> Contract p ()
remove p = lift $ lift $ modify (Set.delete p)

addAll :: Ord p => [p] -> Contract p ()
addAll = mapM_ add

removeAll :: Ord p => [p] -> Contract p ()
removeAll = mapM_ remove

clear :: Contract p ()
clear = lift $ lift $ put Set.empty

output :: Set p -> Contract p ()
output = lift . lift . put

addIf :: (Ord p, Show p) => p -> p -> Contract p ()
addIf p q = branches [ has p >> add q, hasn't p]


addAllIf :: (Ord p, Show p) => p -> [p] -> Contract p ()
addAllIf p qs = branches [ has p >> addAll qs, hasn't p]

removeIf :: (Ord p, Show p) => p -> p -> Contract p ()
removeIf p q = branches [ has p >> remove q, hasn't p]

removeAllIf :: (Ord p, Show p) => p -> [p] -> Contract p ()
removeAllIf p qs = branches [ has p >> removeAll qs, hasn't p]

branchIf :: (Eq p, Show p) => p -> Contract p () -> Contract p () -> Contract p ()
branchIf p a b = branches [ has p >> a, hasn't p >> b ]


branches :: (Eq p, Show p) => [Contract p ()] -> Contract p ()
branches cs = do
  i <- readContractInput
  e <- readContractEdgeName
  o <- readContractOutput
  rs <- mapM (\c -> lift $ lift $ lift $ runContractInternal c e i o) cs
  case catMaybes rs of
    [] -> fail "Nothing"
    [ao] -> setContractOutput ao >> pure ()
    (ao:rest) ->
      if all (==ao) rest
         then setContractOutput ao >> pure ()
         else lift $ lift $ lift $ Left $ renderStyle ourStyle $
               (fromString $ "PermutationEdge " <> e <> " has non-deterministic type")
               $+$ hang "error:" 4 "branches succeeded with different results in each branch."
               $+$ hang "results:" 4 (ppDoc (ao:rest))



ourStyle :: Style
ourStyle = style {lineLength = 80}

