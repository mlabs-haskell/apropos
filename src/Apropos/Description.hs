{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Apropos.Description (
  Description (..),
  VariableRep (V),
  allVariables,
  typeLogic,
  DeepHasDatatypeInfo,
  Generic,
  SOPGeneric,
  HasDatatypeInfo,
  v,
) where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Tree hiding (flatten)

import Data.List.Index (iconcatMap, imap)

import Data.Semigroup (First (First), getFirst)

import Generics.SOP hiding (Generic)

import Generics.SOP qualified as SOP

import GHC.Generics (Generic)

import Data.Tagged (Tagged, unproxy, untag)

import Apropos.Logic
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

{- | A type describing an object.

  You use description types by defining a type that captures the desired properties
  of the object.
-}
class Description d a | d -> a where
  -- | Describe an object; generate a description object from an object.
  describe :: a -> d

  -- | optionally add additional logic constraining valid description types
  additionalLogic :: Formula (VariableRep d)
  additionalLogic = Yes

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
flatten = FlatPack . flatten' (datatypeInfo @a Proxy) . from
  where
    flatten' :: (All2 DeepHasDatatypeInfo xss) => DatatypeInfo xss -> SOP I xss -> Tree ConstructorName
    flatten' ty =
      hcollapse
        . hcliftA2 (Proxy @(All DeepHasDatatypeInfo)) constr (constructorInfo ty)
        . unSOP

    constr :: (All DeepHasDatatypeInfo xs) => ConstructorInfo xs -> NP I xs -> K (Tree ConstructorName) xs
    constr con =
      K
        . Node (constructorName con)
        . hcollapse
        . hcmap (Proxy @DeepHasDatatypeInfo) (K . unFlatPack . flatten . unI)

unflatten :: forall a. (DeepHasDatatypeInfo a) => FlatPack a -> Maybe a
unflatten = fmap to . flatten' (datatypeInfo @a Proxy) . unFlatPack
  where
    flatten' :: forall xss. (All2 DeepHasDatatypeInfo xss) => DatatypeInfo xss -> Tree ConstructorName -> Maybe (SOP I xss)
    flatten' ty tree =
      let injs = injections @xss @(NP I)
       in fmap (SOP . getFirst)
            . mconcat
            . hcollapse
            $ hcliftA2 (Proxy @(All DeepHasDatatypeInfo)) (constr tree) (constructorInfo ty) injs

    constr :: forall xss xs. (All DeepHasDatatypeInfo xs) => Tree ConstructorName -> ConstructorInfo xs -> Injection (NP I) xss xs -> K (Maybe (First (NS (NP I) xss))) xs
    constr tree con (Fn inj)
      | rootLabel tree == constructorName con = K $ do
          flds <- fromList (subForest tree)
          prod <- hsequence . hcmap (Proxy @DeepHasDatatypeInfo) (unflatten . FlatPack . unK) $ flds
          return . First . unK . inj $ prod
      | otherwise = K Nothing

{- | Type of a variable representing the coice of a single constructor within a
 datatype. A datatype is described by a set of such variables, one for each of
 its constructors recursively.

 The representation consists of a string representing the name of the constructor,
 and a path of '(ConstructorName, Int)' pairs, each component representing a
 containing constructor and field number.
-}
data VariableRep a = V
  { vPath :: [(ConstructorName, Int)]
  , vCons :: ConstructorName
  }
  deriving stock (Eq, Ord, Show)

rootVarRep :: ConstructorName -> VariableRep a
rootVarRep = V []

pushVR :: ConstructorName -> Int -> VariableRep a -> VariableRep a
pushVR cn i (V vrs cn') = V ((cn, i) : vrs) cn'

{- | Calculate the set of variables for an object.

 This method operates on any type where
 it and the types of all its fields recursively implement:

 @
   deriving stock (GHC.Generic)
   deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
 @
 = Examples

 >>> descriptionToVariables True
 fromList [V [] "True"]

 >>> descriptionToVariables False
 fromList [V [] "False"]

 >>> descriptionToVariables (True, False)
 fromList [V [] "(,)", V [("(,)",0)] "True", V [("(,)",1)] "False"]

 >>> descriptionToVariables (Just True)
 fromList [V [] "Just", V [("Just",0)] "True"]

 >>> descriptionToVariables (Nothing @(Maybe Bool))
 fromList [V [] "Nothing"]
-}
descriptionToVariables :: (DeepHasDatatypeInfo d) => d -> Set (VariableRep d)
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

variablesToDescription :: (DeepHasDatatypeInfo d) => Set (VariableRep d) -> d
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

    buildMapTree :: Set (VariableRep d) -> MapTree Int ConstructorName
    buildMapTree = Set.foldr insertVar emptyMt

    emptyMt :: MapTree k String
    emptyMt =
      MapNode
        { mapRootLabel = ""
        , mapSubForest = Map.empty
        }

    insertVar :: VariableRep d -> MapTree Int ConstructorName -> MapTree Int ConstructorName
    insertVar (V [] cons) mt =
      mt {mapRootLabel = cons}
    insertVar (V ((_, i) : path) cons) mt =
      mt {mapSubForest = Map.alter (Just . insertVar (V path cons) . fromMaybe emptyMt) i (mapSubForest mt)}

data TwoTree a = TwoNode
  { twoRootLabel :: a
  , twoSubForest :: [[TwoTree a]]
  }
  deriving stock (Functor, Show)

foldTwoTree :: (t -> [[b]] -> b) -> TwoTree t -> b
foldTwoTree f = go
  where
    go (TwoNode x tss) = f x (map (map go) tss)

type Constructor = TwoTree ConstructorName

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
    constr ci = K $ TwoNode (constructorName ci) (hcollapse $ aux @xs)

    aux :: forall xs. (All DeepHasDatatypeInfo xs) => NP (K [Constructor]) xs
    aux = hcpure (Proxy @DeepHasDatatypeInfo) constructorK

    constructorK :: forall a'. DeepHasDatatypeInfo a' => K [Constructor] a'
    constructorK = K $ untag (toConstructors' @a')

{- | Calculate a set of logical constraints governing valid @Set VariableRep@s
 for a type.

 = Examples (simplified)
 >>> typeLogic @Bool
 ExactlyOne [V V [] "False", V V [] "True"]

 >>> typeLogic @(Bool, Bool)
 All [
   ExactlyOne [V V [("(,)",0)] "False", V V [("(,)",0)] "True"],
   ExactlyOne [V V [("(,)",1)] "False", V V [("(,)",1)] "True"]
 ]

 >>> typeLogic @(Either Bool Bool)
 All [
   ExactlyOne [V V [] "Left",V V [] "Right"],
   V V [] "Left" :->: All [
     ExactlyOne [V V [("Left",0)] "False",V V [("Left",0)] "True"],
   ],
   Not (V (V [] "Left")) :->: None [V V [("Left",0)] "False",V V [("Left",0)] "True"],
   V V [] "Right" :->: All [
     ExactlyOne [V V [("Right",0)] "False",V V [("Right",0)] "True"]
   ],
   Not (V (V [] "Right")) :->: None [V V [("Right",0)] "False",V V [("Right",0)] "True"]
 ]
-}
typeLogic :: forall a. (DeepHasDatatypeInfo a) => Formula (VariableRep a)
typeLogic = All . sumLogic $ toConstructors @a
  where
    sumLogic :: [Constructor] -> [Formula (VariableRep a)]
    -- Only one of the constructors can be selected
    sumLogic cs =
      ExactlyOne (map (rootVar . twoRootLabel) cs) :
      -- apply 'prodLogic' to all the fields
      concatMap prodLogic cs

    prodLogic :: Constructor -> [Formula (VariableRep a)]
    prodLogic (TwoNode cn cs) =
      -- for each present constructor, apply 'sumLogic'
      [ rootVar cn :->: All (iconcatMap (\i -> map (pushdownFormula cn i) . sumLogic) cs)
      , -- for each absent constructor, none of the constructors of its fields can be selected
        Not (rootVar cn) :->: None (iconcatMap (\i -> map (pushdownFormula cn i . rootVar . twoRootLabel)) cs)
      ]

    pushdownFormula :: ConstructorName -> Int -> Formula (VariableRep a) -> Formula (VariableRep a)
    pushdownFormula cn i = fmap (pushVR cn i)

rootVar :: ConstructorName -> Formula (VariableRep a)
rootVar = Var . rootVarRep

{- | Enumerate all the variables of a type.

  = Examples

  >>> allVariables @Bool
  fromList [V [] "GHC.Types.False",V [] "GHC.Types.True"]

  >>> allVariables @(Bool, Bool)
  fromList
  [ V [] "GHC.Tuple.(,)"
  , V [("GHC.Tuple.(,)",0)] "GHC.Types.False"
  , V [("GHC.Tuple.(,)",0)] "GHC.Types.True"
  , V [("GHC.Tuple.(,)",1)] "GHC.Types.False"
  , V [("GHC.Tuple.(,)",1)] "GHC.Types.True"
 ]

 >>> allVariables @(Maybe Bool)
 fromList
  [ V [] "GHC.Maybe.Just"
  , V [] "GHC.Maybe.Nothing"
  , V [("GHC.Maybe.Just",0)] "GHC.Types.False"
  , V [("GHC.Maybe.Just",0)] "GHC.Types.True"
  ]
-}
allVariables :: forall a. (DeepHasDatatypeInfo a) => Set (VariableRep a)
allVariables = Set.unions . map allVariables' $ toConstructors @a
  where
    allVariables' :: Constructor -> Set (VariableRep a)
    allVariables' =
      foldTwoTree
        ( \cn flds ->
            Set.singleton (rootVarRep cn)
              <> Set.unions (imap (\i -> Set.map (pushVR cn i) . Set.unions) flds)
        )

v :: [(ConstructorName, Int)] -> ConstructorName -> Formula (VariableRep a)
v path = Var . V path

instance (DeepHasDatatypeInfo d, Description d a) => Strategy (VariableRep d) a where
  type Properties (VariableRep d) = d

  logic = typeLogic :&&: additionalLogic

  universe = Set.toList allVariables

  toProperties = describe

  propertiesToVariables = descriptionToVariables

  variablesToProperties = variablesToDescription

type SOPGeneric = SOP.Generic

