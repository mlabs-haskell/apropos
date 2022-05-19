{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Apropos.Description (
  Description (..),
  VariableRep,
  variableSet,
  allVariables,
  typeLogic,
  DeepHasDatatypeInfo,
) where

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Tree (Tree (Node), foldTree)

import Data.List.Index (iconcatMap, imap)

import Generics.SOP (
  All,
  All2,
  ConstructorInfo (Constructor, Infix, Record),
  ConstructorName,
  DatatypeInfo,
  Generic (Code, from),
  HCollapse (hcollapse),
  HPure (hcpure),
  HasDatatypeInfo (datatypeInfo),
  I,
  K (..),
  NP,
  Proxy (Proxy),
  SListI,
  SOP,
  constructorInfo,
  constructorName,
  hcliftA2,
  hcmap,
  hmap,
  moduleName,
  unI,
  unSOP,
 )

import Data.Tagged (Tagged, unproxy, untag)

import SAT.MiniSat (Formula ((:&&:), (:++:), (:->:), (:<->:), (:||:)))
import SAT.MiniSat qualified as SAT

{- | A type describing an object.

  You use description types by defining a type that captures the desired properties
  of the object.
-}
class Description a d | d -> a where
  -- | Describe an object; generate a description object from an object.
  describe :: a -> d

  -- | optionally add additional logic constraining valid description types
  additionalLogic :: Formula (VariableRep a)
  additionalLogic = SAT.Yes

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

{- | Generically construct a 'FlatPack'.

 This method operates on any type where
 it and the types of all its fields recursively implement:

 @
   deriving stock (GHC.Generic)
   deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
 @
-}
flatpack :: forall a. (DeepHasDatatypeInfo a) => a -> FlatPack a
flatpack = flatpack' (datatypeInfo (Proxy @a)) . from
  where
    flatpack' :: (All2 DeepHasDatatypeInfo xss) => DatatypeInfo xss -> SOP I xss -> FlatPack a
    flatpack' ty =
      hcollapse
        . hcliftA2 (Proxy @(All DeepHasDatatypeInfo)) constr (qualifiedConstructorInfo ty)
        . unSOP

    constr :: (All DeepHasDatatypeInfo xs) => ConstructorInfo xs -> NP I xs -> K (FlatPack a) xs
    constr con =
      K
        . FlatPack
        . Node (constructorName con)
        . hcollapse
        . hcmap (Proxy @DeepHasDatatypeInfo) (K . unFlatPack . flatpack . unI)

{- | Type of a variable representing the coice of a single constructor within a
 datatype. A datatype is described by a set of such variables, one for each of
 its constructors recursively.

 The representation consists of a string representing the name of the constructor,
 and a path of '(ConstructorName, Int)' pairs, each component representing a
 containing constructor and field number.
-}
data VariableRep a = Var [(ConstructorName, Int)] ConstructorName
  deriving stock (Eq, Ord, Show)

rootVarRep :: ConstructorName -> VariableRep a
rootVarRep = Var []

pushVR :: ConstructorName -> Int -> VariableRep a -> VariableRep a
pushVR cn i (Var vrs cn') = Var ((cn, i) : vrs) cn'

{- | Calculate the set of variables for an object.

 This method operates on any type where
 it and the types of all its fields recursively implement:

 @
   deriving stock (GHC.Generic)
   deriving anyclass (Generics.SOP.Generic, Generics.SOP.HasDatatypeInfo)
 @
 = Examples

 >>> variableSet True
 fromList [Var [] "True"]

 >>> variableSet False
 fromList [Var [] "False"]

 >>> variableSet (True, False)
 fromList [Var [] "(,)", Var [("(,)",0)] "True", Var [("(,)",1)] "False"]

 >>> variableSet (Just True)
 fromList [Var [] "Just", Var [("Just",0)] "True"]

 >>> variableSet (Nothing @(Maybe Bool))
 fromList [Var [] "Nothing"]
-}
variableSet :: (DeepHasDatatypeInfo a) => a -> Set (VariableRep a)
variableSet = constructorsToVariables . unFlatPack . flatpack

data TwoTree a = TwoNode
  { rootLabel :: a
  , subForest :: [[TwoTree a]]
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
          . qualifiedConstructorInfo
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
 ExactlyOne [Var Var [] "False", Var Var [] "True"]

 >>> typeLogic @(Bool, Bool)
 All [
   ExactlyOne [Var Var [("(,)",0)] "False", Var Var [("(,)",0)] "True"],
   ExactlyOne [Var Var [("(,)",1)] "False", Var Var [("(,)",1)] "True"]
 ]

 >>> typeLogic @(Either Bool Bool)
 All [
   ExactlyOne [Var Var [] "Left",Var Var [] "Right"],
   Var Var [] "Left" :->: All [
     ExactlyOne [Var Var [("Left",0)] "False",Var Var [("Left",0)] "True"],
   ],
   Not (Var (Var [] "Left")) :->: None [Var Var [("Left",0)] "False",Var Var [("Left",0)] "True"],
   Var Var [] "Right" :->: All [
     ExactlyOne [Var Var [("Right",0)] "False",Var Var [("Right",0)] "True"]
   ],
   Not (Var (Var [] "Right")) :->: None [Var Var [("Right",0)] "False",Var Var [("Right",0)] "True"]
 ]
-}
typeLogic :: forall a. (DeepHasDatatypeInfo a) => Formula (VariableRep a)
typeLogic = SAT.All . sumLogic $ toConstructors @a
  where
    sumLogic :: [Constructor] -> [Formula (VariableRep a)]
    -- Only one of the constructors can be selected
    sumLogic cs =
      SAT.ExactlyOne (map (rootVar . rootLabel) cs) :
      -- apply 'prodLogic' to all the fields
      concatMap prodLogic cs

    prodLogic :: Constructor -> [Formula (VariableRep a)]
    prodLogic (TwoNode cn cs) =
      -- for each present constructor, apply 'sumLogic'
      [ rootVar cn :->: SAT.All (iconcatMap (\i -> map (pushdownFormula cn i) . sumLogic) cs)
      , -- for each absent constructor, none of the constructors of its fields can be selected
        SAT.Not (rootVar cn) :->: SAT.None (iconcatMap (\i -> map (pushdownFormula cn i . rootVar . rootLabel)) cs)
      ]

    pushdownFormula :: ConstructorName -> Int -> Formula (VariableRep a) -> Formula (VariableRep a)
    pushdownFormula cn i = mapFormula (pushVR cn i)

    mapFormula :: (v -> v) -> Formula v -> Formula v
    mapFormula f (SAT.Var v) = SAT.Var (f v)
    mapFormula _ SAT.Yes = SAT.Yes
    mapFormula _ SAT.No = SAT.No
    mapFormula f (SAT.Not a) = SAT.Not (mapFormula f a)
    mapFormula f (a :&&: b) = mapFormula f a :&&: mapFormula f b
    mapFormula f (a :||: b) = mapFormula f a :||: mapFormula f b
    mapFormula f (a :++: b) = mapFormula f a :++: mapFormula f b
    mapFormula f (a :->: b) = mapFormula f a :->: mapFormula f b
    mapFormula f (a :<->: b) = mapFormula f a :<->: mapFormula f b
    mapFormula f (SAT.All fs) = SAT.All (map (mapFormula f) fs)
    mapFormula f (SAT.Some fs) = SAT.Some (map (mapFormula f) fs)
    mapFormula f (SAT.None fs) = SAT.None (map (mapFormula f) fs)
    mapFormula f (SAT.ExactlyOne fs) = SAT.ExactlyOne (map (mapFormula f) fs)
    mapFormula f (SAT.AtMostOne fs) = SAT.AtMostOne (map (mapFormula f) fs)
    mapFormula f (SAT.Let a f') = SAT.Let (mapFormula f a) f'
    mapFormula _ (SAT.Bound i) = SAT.Bound i

    rootVar :: ConstructorName -> Formula (VariableRep a)
    rootVar = SAT.Var . rootVarRep

{- | Enumerate all the variables of a type.

  = Examples

  >>> allVariables @Bool
  fromList [Var [] "GHC.Types.False",Var [] "GHC.Types.True"]

  >>> allVariables @(Bool, Bool)
  fromList
  [ Var [] "GHC.Tuple.(,)"
  , Var [("GHC.Tuple.(,)",0)] "GHC.Types.False"
  , Var [("GHC.Tuple.(,)",0)] "GHC.Types.True"
  , Var [("GHC.Tuple.(,)",1)] "GHC.Types.False"
  , Var [("GHC.Tuple.(,)",1)] "GHC.Types.True"
 ]

 >>> allVariables @(Maybe Bool)
 fromList
  [ Var [] "GHC.Maybe.Just"
  , Var [] "GHC.Maybe.Nothing"
  , Var [("GHC.Maybe.Just",0)] "GHC.Types.False"
  , Var [("GHC.Maybe.Just",0)] "GHC.Types.True"
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

constructorsToVariables :: Tree ConstructorName -> Set (VariableRep a)
constructorsToVariables =
  foldTree
    ( \cn flds ->
        Set.singleton (rootVarRep cn)
          <> Set.unions (imap (Set.map . pushVR cn) flds)
    )

qualifiedConstructorInfo :: (SListI xss) => DatatypeInfo xss -> NP ConstructorInfo xss
qualifiedConstructorInfo di = hmap adjust (constructorInfo di)
  where
    adjust :: ConstructorInfo xs -> ConstructorInfo xs
    adjust (Constructor cn) = Constructor (qualify cn)
    adjust (Infix cn ass fix) = Infix (qualify cn) ass fix
    adjust (Record cn fis) = Record (qualify cn) fis

    qualify :: ConstructorName -> ConstructorName
    qualify cn = moduleName di ++ "." ++ cn
