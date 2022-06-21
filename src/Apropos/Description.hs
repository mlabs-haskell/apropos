{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Apropos.Description (
  Description (..),
  Attribute (Attr),
  typeLogic,
  DeepHasDatatypeInfo,
  Generic,
  SOPGeneric,
  HasDatatypeInfo,
  attr,
  variablesToDescription,
  logic,
  allAttributes,
  enumerateScenariosWhere,
  scenarios,
  satisfies,
) where

import Data.String (IsString (fromString))

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Tree hiding (flatten)

import Data.List.Index (iconcatMap, imap)

import Data.Semigroup (First (First), getFirst)

import Generics.SOP hiding (Generic)

import Generics.SOP qualified as SOP

import GHC.Generics (Generic)

import Data.Tagged (Tagged, unproxy, untag)

import Apropos.Formula
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Hedgehog (MonadGen)

{- | A type describing an object.

  You use description types by defining a type that captures the desired properties
  of the object.
-}
class Description d a | d -> a where
  -- | Describe a value; generate a description object from a value.
  describe :: a -> d

  -- | Optionally add additional logic constraining valid description values
  refineDescription :: Formula (Attribute d)
  refineDescription = Yes

  -- | Generate test values matching a description.
  genDescribed :: (MonadGen m) => d -> m a

{- | A constraint asserting that a type and the types of all its fields recursively
 implement 'HasDatatypeInfo'.
-}
class (HasDatatypeInfo a, All2 DeepHasDatatypeInfo (Code a)) => DeepHasDatatypeInfo a

instance (HasDatatypeInfo a, All2 DeepHasDatatypeInfo (Code a)) => DeepHasDatatypeInfo a

{- | A datatype-agnostic representation of an object, consisting of a string
 representing the constructor and a list of recursive structures representing
 the fields.
 The type parameter is unused except to add a bit of type safety.
-}
newtype FlatPack a = FlatPack {unFlatPack :: Tree ConstructorName}
  deriving newtype (Show)

{- | Generically construct a 'FlatPack'.

 This method operates on any type where
 it and the types of all its fields recursively implement:

 @
   deriving stock (GHC.Generic)
   deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
 @
-}
flatten :: forall a. (DeepHasDatatypeInfo a) => a -> FlatPack a
flatten =
  FlatPack
    . hcollapse
    . hcliftA2 (Proxy @(All DeepHasDatatypeInfo)) constr (constructorInfo (datatypeInfo @a Proxy))
    . unSOP
    . from
  where
    constr :: (All DeepHasDatatypeInfo xs) => ConstructorInfo xs -> NP I xs -> K (Tree ConstructorName) xs
    constr con =
      K
        . Node (constructorName con)
        . hcollapse
        . hcmap (Proxy @DeepHasDatatypeInfo) (K . unFlatPack . flatten . unI)

unflatten :: forall a. (DeepHasDatatypeInfo a) => FlatPack a -> Maybe a
unflatten fp =
  fmap (to . SOP . getFirst)
    . mconcat
    . hcollapse
    $ hcliftA2
      (Proxy @(All DeepHasDatatypeInfo))
      (constr (unFlatPack fp))
      (constructorInfo (datatypeInfo @a Proxy))
      (injections @(Code a) @(NP I))
  where
    constr :: forall xs. (All DeepHasDatatypeInfo xs) => Tree ConstructorName -> ConstructorInfo xs -> Injection (NP I) (Code a) xs -> K (Maybe (First (NS (NP I) (Code a)))) xs
    constr tree con (Fn inj)
      | rootLabel tree == constructorName con = K $ do
          flds <- fromList (subForest tree)
          prod <- hsequence . hcmap (Proxy @DeepHasDatatypeInfo) (unflatten . FlatPack . unK) $ flds
          return . First . unK . inj $ prod
      | otherwise = K Nothing

{- | Type of a variable representing the choice of a single constructor within a
 datatype. A datatype is described by a set of such variables, one for each of
 its constructors recursively.

 The representation consists of a string representing the name of the constructor,
 and a path of '(ConstructorName, Int)' pairs, each component representing a
 containing constructor and field number.
-}
data Attribute d = Attr
  { attrPath :: [(ConstructorName, Int)]
  , attrConstr :: ConstructorName
  }
  deriving stock (Eq, Ord, Show, Generic)

data FieldSelector
  = Index Int
  | RecordField FieldName
  deriving stock (Eq, Ord)

instance Show FieldSelector where
  show (Index i) = show i
  show (RecordField l) = show l

instance Num FieldSelector where
  fromInteger = Index . fromInteger
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

instance IsString FieldSelector where
  fromString = RecordField

rootVarRep :: ConstructorName -> Attribute d
rootVarRep = Attr []

pushVR :: ConstructorName -> Int -> Attribute d -> Attribute d
pushVR cn i (Attr vrs cn') = Attr ((cn, i) : vrs) cn'

{- | Calculate the set of variables for an object.

 This method operates on any type where
 it and the types of all its fields recursively implement:

 @
   deriving stock (GHC.Generic)
   deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
 @
 = Examples

 >>> descriptionToVariables True
 fromList [Attr [] "True"]

 >>> descriptionToVariables False
 fromList [Attr [] "False"]

 >>> descriptionToVariables (True, False)
 fromList [Attr [] "(,)", Attr [("(,)",0)] "True", Attr [("(,)",1)] "False"]

 >>> descriptionToVariables (Just True)
 fromList [Attr [] "Just", Attr [("Just",0)] "True"]

 >>> descriptionToVariables (Nothing @(Maybe Bool))
 fromList [Attr [] "Nothing"]
-}
descriptionToVariables :: (DeepHasDatatypeInfo d) => d -> Set (Attribute d)
descriptionToVariables =
  foldTree
    ( \cn flds ->
        Set.singleton (rootVarRep cn)
          <> Set.unions (imap (Set.map . pushVR cn) flds)
    )
    . unFlatPack
    . flatten

data MapTree k a = MapNode
  { mapRootLabel :: a
  , mapSubForest :: Map k (MapTree k a)
  }
  deriving stock (Show)

variablesToDescription :: (DeepHasDatatypeInfo d) => Set (Attribute d) -> d
variablesToDescription s =
  let tree = collapseMapTree . buildMapTree $ s
   in case unflatten . FlatPack $ tree of
        Nothing -> error ("Invalid FlatPack " ++ drawTree tree)
        Just a -> a
  where
    collapseMapTree :: MapTree i a -> Tree a
    collapseMapTree mt =
      Node
        { rootLabel = mapRootLabel mt
        , subForest =
            map snd
              . Map.toAscList
              . Map.map collapseMapTree
              . mapSubForest
              $ mt
        }

    buildMapTree :: Set (Attribute d) -> MapTree Int ConstructorName
    buildMapTree = Set.foldr insertVar emptyMt

    emptyMt :: MapTree k String
    emptyMt =
      MapNode
        { mapRootLabel = ""
        , mapSubForest = Map.empty
        }

    insertVar :: Attribute d -> MapTree Int ConstructorName -> MapTree Int ConstructorName
    insertVar (Attr [] cons) mt =
      mt {mapRootLabel = cons}
    insertVar (Attr ((_, i) : path) cons) mt =
      mt {mapSubForest = Map.alter (Just . insertVar (Attr path cons) . fromMaybe emptyMt) i (mapSubForest mt)}

data TwoTree a = TwoNode
  { twoRootLabel :: a
  , twoSubForest :: [[TwoTree a]]
  }
  deriving stock (Functor, Show)

foldTwoTree :: (t -> [[b]] -> b) -> TwoTree t -> b
foldTwoTree f = go
  where
    go (TwoNode x tss) = f x (map (map go) tss)

type Constructor = TwoTree ConsInfo

data ConsInfo = ConsInfo
  { consName :: ConstructorName
  , consFields :: [FieldName]
  }
  deriving stock (Show)

toConstructors :: forall a. (DeepHasDatatypeInfo a) => [Constructor]
toConstructors = untag (toConstructors' @a)
  where
    toConstructors' :: forall a'. (DeepHasDatatypeInfo a') => Tagged a' [Constructor]
    toConstructors' =
      unproxy $
        hcollapse
          . hcmap (Proxy @(All DeepHasDatatypeInfo)) constr
          . constructorInfo
          . datatypeInfo

    constr :: forall xs. (All DeepHasDatatypeInfo xs) => ConstructorInfo xs -> K Constructor xs
    constr ci = K $ TwoNode (ConsInfo {consName = constructorName ci, consFields = fields ci}) (hcollapse $ aux @xs)

    fields :: ConstructorInfo xs -> [FieldName]
    fields (Record _ flds) = hcollapse . hmap (K . fieldName) $ flds
    fields _ = []

    aux :: forall xs. (All DeepHasDatatypeInfo xs) => NP (K [Constructor]) xs
    aux = hcpure (Proxy @DeepHasDatatypeInfo) constructorK

    constructorK :: forall a'. DeepHasDatatypeInfo a' => K [Constructor] a'
    constructorK = K $ untag (toConstructors' @a')

{- | Calculate a set of logical constraints governing valid @Set Attribute@s
 for a type.

 = Examples (simplified)
 >>> typeLogic @Bool
 ExactlyOne [Attr Attr [] "False", Attr Attr [] "True"]

 >>> typeLogic @(Bool, Bool)
 All [
   ExactlyOne [Attr Attr [("(,)",0)] "False", Attr Attr [("(,)",0)] "True"],
   ExactlyOne [Attr Attr [("(,)",1)] "False", Attr Attr [("(,)",1)] "True"]
 ]

 >>> typeLogic @(Either Bool Bool)
 All [
   ExactlyOne [Attr Attr [] "Left",Attr Attr [] "Right"],
   Attr Attr [] "Left" :->: All [
     ExactlyOne [Attr Attr [("Left",0)] "False",Attr Attr [("Left",0)] "True"],
   ],
   Not (Attr (Attr [] "Left")) :->: None [Attr Attr [("Left",0)] "False",Attr Attr [("Left",0)] "True"],
   Attr Attr [] "Right" :->: All [
     ExactlyOne [Attr Attr [("Right",0)] "False",Attr Attr [("Right",0)] "True"]
   ],
   Not (Attr (Attr [] "Right")) :->: None [Attr Attr [("Right",0)] "False",Attr Attr [("Right",0)] "True"]
 ]
-}
typeLogic :: forall d. (DeepHasDatatypeInfo d) => Formula (Attribute d)
typeLogic = All . sumLogic $ toConstructors @d
  where
    sumLogic :: [Constructor] -> [Formula (Attribute d)]
    sumLogic cs =
      -- Only one of the top-level constructors can be selected
      ExactlyOne (subVars cs) :
      -- apply 'prodLogic' to all the fields
      concatMap prodLogic cs

    prodLogic :: Constructor -> [Formula (Attribute d)]
    prodLogic (TwoNode (ConsInfo cn _) cs) =
      -- for each present constructor, one of the constructors of each of its fields can be selected
      [ rootVar cn :->: (All . imap (\i -> ExactlyOne . pushedSubvars cn i) $ cs)
      , -- for each absent constructor, none of the constructors of any of its fields can be selected
        Not (rootVar cn) :->: (None . iconcatMap (pushedSubvars cn) $ cs)
        -- recurse
      ]
        ++ iconcatMap (\i -> map (fmap $ pushVR cn i) . concatMap prodLogic) cs

    pushedSubvars :: ConstructorName -> Int -> [Constructor] -> [Formula (Attribute d)]
    pushedSubvars cn i = map (fmap (pushVR cn i)) . subVars

    subVars :: [Constructor] -> [Formula (Attribute d)]
    subVars = map (rootVar . consName . twoRootLabel)

rootVar :: ConstructorName -> Formula (Attribute d)
rootVar = Var . rootVarRep

{- | Enumerate all the variables of a type.

  = Examples

  >>> allAttributes @Bool
  fromList [Attr [] "GHC.Types.False",Attr [] "GHC.Types.True"]

  >>> allAttributes @(Bool, Bool)
  fromList
  [ Attr [] "GHC.Tuple.(,)"
  , Attr [("GHC.Tuple.(,)",0)] "GHC.Types.False"
  , Attr [("GHC.Tuple.(,)",0)] "GHC.Types.True"
  , Attr [("GHC.Tuple.(,)",1)] "GHC.Types.False"
  , Attr [("GHC.Tuple.(,)",1)] "GHC.Types.True"
 ]

 >>> allAttributes @(Maybe Bool)
 fromList
  [ Attr [] "GHC.Maybe.Just"
  , Attr [] "GHC.Maybe.Nothing"
  , Attr [("GHC.Maybe.Just",0)] "GHC.Types.False"
  , Attr [("GHC.Maybe.Just",0)] "GHC.Types.True"
  ]
-}
allAttributes :: forall d. (DeepHasDatatypeInfo d) => Set (Attribute d)
allAttributes = Set.unions . map allAttributes' $ toConstructors @d
  where
    allAttributes' :: Constructor -> Set (Attribute d)
    allAttributes' =
      foldTwoTree
        ( \(ConsInfo cn _) flds ->
            Set.singleton (rootVarRep cn)
              <> Set.unions (imap (\i -> Set.map (pushVR cn i) . Set.unions) flds)
        )

attr :: forall d. (DeepHasDatatypeInfo d) => [(ConstructorName, FieldSelector)] -> ConstructorName -> Formula (Attribute d)
attr path = Var . resolveFS path
  where
    resolveFS :: [(ConstructorName, FieldSelector)] -> ConstructorName -> Attribute d
    resolveFS p = Attr (resPath cs p)
      where
        resPath :: [Constructor] -> [(ConstructorName, FieldSelector)] -> [(ConstructorName, Int)]
        resPath _ [] = []
        resPath cs' ((con', Index i) : path') = (con', i) : resPath ((!! i) . twoSubForest . findConstructor con' $ cs') path'
        resPath cs' ((con', RecordField fld) : path') = resPath cs' ((con', Index (fromJust . elemIndex fld . consFields . twoRootLabel $ findConstructor con' cs')) : path')

        findConstructor :: ConstructorName -> [Constructor] -> Constructor
        findConstructor con' = head . filter ((== con') . consName . twoRootLabel)

        cs :: [Constructor]
        cs = toConstructors @d

logic :: (Description d a, DeepHasDatatypeInfo d) => Formula (Attribute d)
logic = typeLogic :&&: refineDescription

enumerateScenariosWhere :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Formula (Attribute d) -> Set (Set (Attribute d))
enumerateScenariosWhere holds = enumerateSolutions $ logic :&&: holds

scenarios :: forall d a. (Description d a, DeepHasDatatypeInfo d) => Set (Set (Attribute d))
scenarios = enumerateScenariosWhere Yes

satisfies :: forall d. (DeepHasDatatypeInfo d) => Formula (Attribute d) -> (d -> Bool)
satisfies f s = satisfiable $ f :&&: All (Var <$> Set.toList set) :&&: None (Var <$> Set.toList unset)
  where
    set :: Set (Attribute d)
    set = descriptionToVariables s
    unset :: Set (Attribute d)
    unset = Set.difference allAttributes set

type SOPGeneric = SOP.Generic
