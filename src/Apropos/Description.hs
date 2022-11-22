module Apropos.Description (
  Description (describe, refineDescription, genDescribed),
  Formula,
  Formula' (
    Yes,
    No,
    Not,
    (:&&:),
    (:||:),
    (:++:),
    (:->:),
    (:<->:),
    All,
    Some,
    None,
    ExactlyOne,
    AtMostOne
  ),
  Attribute (Attr),
  FieldSelector,
  typeLogic,
  DeepGeneric,
  attr,
  variablesToDescription,
  logic,
  allAttributes,
  enumerateScenariosWhere,
  scenarios,
  satisfies,
) where

import Control.Monad.State

import Data.String (IsString (fromString))

import Data.Set (Set)
import Data.Set qualified as Set

import Data.Tree (Tree)
import Data.Tree qualified as Tree

import Data.Proxy (Proxy (Proxy))

import Data.Semigroup (First (First), getFirst)

import Data.Vector (Vector)
import Data.Vector qualified as Vector

import Generics.SOP (All, I, K (K), NP, NS, SOP (SOP), unI, unK, unSOP)
import Generics.SOP qualified as SOP
import Generics.SOP.GGP (
  GCode,
  GDatatypeInfo,
  GFrom,
  GTo,
  gdatatypeInfo,
  gfrom,
  gto,
 )

import GHC.Generics (Generic)

import Data.Tagged (Tagged, unproxy, untag)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)

import Data.Kind (Constraint, Type)
import Hedgehog (MonadGen)
import SAT.MiniSat qualified as MiniSAT

{- | Define a description type - an ADT that captures interesting properties of
a type.

Defining an instance of this typeclass is the first step for writing apropos
tests.

Instances of this typeclass should observe the following law:

@
forall d. forAll (genDescribed d) >>= (\\a -> describe a === d)
@

'Apropos.selfTest' is provided for testing adherence to this law.

The type @d@ and the types of all its fields recursively must derive 'Generic'.
-}
type Description :: Type -> Type -> Constraint
class (DeepGeneric d) => Description d a | d -> a where
  -- | Describe a value
  --
  --  Generate a description from a value.
  describe :: a -> d

  -- | Add logic constraining valid description values
  --
  --  Not all constructible description values may be valid. This optional
  --  method allows you to specify which. See 'Formula' for more information how
  -- to specify these values.
  refineDescription :: Formula d
  refineDescription = Yes

  -- | Generate test values matching a description.
  --
  --  You specify a Hedgehog 'Hedgehog.Gen' that generates a value for a given
  -- description.
  genDescribed :: forall (m :: Type -> Type). (MonadGen m) => d -> m a

{- |

This somewhat strange constraint enforces that description types and all their
fields implement 'GHC.Generic'and are of a suitable shape. This is used by many
Apropos functions. You can probably ignore this, but it may be helful for
building combinators on top of Apropos.
-}
type DeepGeneric :: Type -> Constraint
class
  ( Generic a
  , GDatatypeInfo a
  , GFrom a
  , GTo a
  , SOP.All2 DeepGeneric (GCode a)
  ) =>
  DeepGeneric a

instance
  ( Generic a
  , GDatatypeInfo a
  , GFrom a
  , GTo a
  , SOP.All2 DeepGeneric (GCode a)
  ) =>
  DeepGeneric a

{- | A datatype-agnostic representation of an object, consisting of a string
 representing the constructor and a list of recursive structures representing
 the fields.

 The type parameter is unused except to add a bit of type safety.
-}
type FlatPack :: Type -> Type
newtype FlatPack a = FlatPack {unFlatPack :: Tree SOP.ConstructorName}
  deriving newtype (Show)

{- | Generically construct a 'FlatPack'.

 This method operates on any type where it and the types of all its fields
 recursively implement @GHC.Generic@.
-}
flatten :: forall (a :: Type). (DeepGeneric a) => a -> FlatPack a
flatten =
  FlatPack
    . SOP.hcollapse
    . SOP.hcliftA2
      (Proxy @(All DeepGeneric))
      constr
      (SOP.constructorInfo (gdatatypeInfo (Proxy @a)))
    . unSOP
    . gfrom
  where
    constr ::
      forall (xs :: [Type]).
      (All DeepGeneric xs) =>
      SOP.ConstructorInfo xs ->
      NP I xs ->
      K (Tree SOP.ConstructorName) xs
    constr con =
      K
        . Tree.Node (SOP.constructorName con)
        . SOP.hcollapse
        . SOP.hcmap (Proxy @DeepGeneric) (K . unFlatPack . flatten . unI)

unflatten :: forall (a :: Type). (DeepGeneric a) => FlatPack a -> Maybe a
unflatten fp =
  fmap (gto . SOP . getFirst)
    . mconcat
    . SOP.hcollapse
    $ SOP.hcliftA2
      (Proxy @(All DeepGeneric))
      (constr (unFlatPack fp))
      (SOP.constructorInfo (gdatatypeInfo (Proxy @a)))
      (SOP.injections @(GCode a) @(NP I))
  where
    constr ::
      forall (xs :: [Type]).
      (All DeepGeneric xs) =>
      Tree SOP.ConstructorName ->
      SOP.ConstructorInfo xs ->
      SOP.Injection (NP I) (GCode a) xs ->
      K (Maybe (First (NS (NP I) (GCode a)))) xs
    constr tree con (SOP.Fn inj)
      | Tree.rootLabel tree == SOP.constructorName con = K $ do
          flds <- SOP.fromList (Tree.subForest tree)
          prod <-
            SOP.hsequence
              . SOP.hcmap (Proxy @DeepGeneric) (unflatten . FlatPack . unK)
              $ flds
          return . First . unK . inj $ prod
      | otherwise = K Nothing

type Attribute :: Type -> Type -> Type
data Attribute i d = Attr
  { attrPath :: Vector (SOP.ConstructorName, i)
  , attrConstr :: SOP.ConstructorName
  }
  deriving stock (Eq, Ord, Generic)

{- |
  Type for specifying a field in a constructor, in an attribute path.

  This type is a little bit magic in that it is an instance of both 'Num' and
  'IsString', allowing you to specify both field names and integer indices using
  literal syntax.

  See the examples for 'allAttributes' for more information.
-}
type FieldSelector :: Type
data FieldSelector
  = Index Int
  | RecordField SOP.FieldName
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

rootVarRep ::
  forall (i :: Type) (d :: Type).
  SOP.ConstructorName ->
  Attribute i d
rootVarRep = Attr Vector.empty

pushVR ::
  forall (i :: Type) (d :: Type).
  SOP.ConstructorName ->
  i ->
  Attribute i d ->
  Attribute i d
pushVR cn i (Attr vrs cn') = Attr ((cn, i) `Vector.cons` vrs) cn'

{- The examples are now out of date; there is no 'Show' instance for
'Attribute'.

This has not been considered a priority to fix, as these functions (and the
'Attribute' type) are not currently public.
-}

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
descriptionToVariables ::
  forall (d :: Type). (DeepGeneric d) => d -> Set (Attribute Int d)
descriptionToVariables =
  Tree.foldTree
    ( \cn flds ->
        Set.singleton (rootVarRep cn)
          <> Set.unions
            (Vector.imap (Set.map . pushVR cn) (Vector.fromList flds))
    )
    . unFlatPack
    . flatten

type MapTree :: Type -> Type -> Type
data MapTree k a = MapNode
  { mapRootLabel :: a
  , mapSubForest :: Map k (MapTree k a)
  }
  deriving stock (Show)

variablesToDescription ::
  forall (d :: Type). (DeepGeneric d) => Set (Attribute Int d) -> d
variablesToDescription s =
  let tree = collapseMapTree . buildMapTree $ s
   in case unflatten . FlatPack $ tree of
        Nothing -> error ("Invalid FlatPack " ++ Tree.drawTree tree)
        Just a -> a
  where
    collapseMapTree :: forall (i :: Type) (a :: Type). MapTree i a -> Tree a
    collapseMapTree mt =
      Tree.Node
        { rootLabel = mapRootLabel mt
        , subForest =
            map snd
              . Map.toAscList
              . Map.map collapseMapTree
              . mapSubForest
              $ mt
        }

    buildMapTree :: Set (Attribute Int d) -> MapTree Int SOP.ConstructorName
    buildMapTree = Set.foldr insertVar emptyMt

    emptyMt :: forall (k :: Type). MapTree k SOP.ConstructorName
    emptyMt =
      MapNode
        { mapRootLabel = ""
        , mapSubForest = Map.empty
        }

    insertVar ::
      Attribute Int d ->
      MapTree Int SOP.ConstructorName ->
      MapTree Int SOP.ConstructorName
    insertVar (Attr v cons) mt
      | Nothing <- Vector.uncons v = mt {mapRootLabel = cons}
      | Just ((_, i), path) <- Vector.uncons v =
          mt
            { mapSubForest =
                Map.alter
                  (Just . insertVar (Attr path cons) . fromMaybe emptyMt)
                  i
                  (mapSubForest mt)
            }

type Constructor :: Type
data Constructor = Constructor
  { constructorInfo :: ConsInfo
  , subConstructors :: Vector (Vector Constructor)
  }
  deriving stock (Show)

foldConstructor ::
  forall (a :: Type). (ConsInfo -> Vector (Vector a) -> a) -> Constructor -> a
foldConstructor f = go
  where
    go (Constructor x tss) = f x (fmap (fmap go) tss)

type ConsInfo :: Type
data ConsInfo = ConsInfo
  { consName :: SOP.ConstructorName
  , consFields :: Maybe (Vector SOP.FieldName)
  }
  deriving stock (Show)

toConstructors ::
  forall (a :: Type). (DeepGeneric a) => Proxy a -> Vector Constructor
toConstructors _ = untag (toConstructors' @a)
  where
    toConstructors' ::
      forall (a' :: Type). (DeepGeneric a') => Tagged a' (Vector Constructor)
    toConstructors' =
      unproxy $
        Vector.fromList
          . SOP.hcollapse
          . SOP.hcmap (Proxy @(All DeepGeneric)) constr
          . SOP.constructorInfo
          . gdatatypeInfo

    constr ::
      forall (xs :: [Type]).
      (All DeepGeneric xs) =>
      SOP.ConstructorInfo xs ->
      K Constructor xs
    constr ci =
      K $
        Constructor
          (ConsInfo {consName = SOP.constructorName ci, consFields = fields ci})
          (Vector.fromList . SOP.hcollapse $ aux @xs)

    fields ::
      forall (xs :: [Type]).
      SOP.ConstructorInfo xs ->
      Maybe (Vector SOP.FieldName)
    fields (SOP.Record _ flds) =
      Just
        . Vector.fromList
        . SOP.hcollapse
        . SOP.hmap (K . SOP.fieldName)
        $ flds
    fields _ = Nothing

    aux ::
      forall (xs :: [Type]).
      (All DeepGeneric xs) =>
      NP (K (Vector Constructor)) xs
    aux = SOP.hcpure (Proxy @DeepGeneric) constructorK

    constructorK ::
      forall (a' :: Type). DeepGeneric a' => K (Vector Constructor) a'
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
  Not (Attr (Attr [] "Left")) :->:
    None [Attr Attr [("Left",0)] "False",Attr Attr [("Left",0)] "True"],
  Attr Attr [] "Right" :->: All [
    ExactlyOne [Attr Attr [("Right",0)] "False",Attr Attr [("Right",0)] "True"]
  ],
  Not (Attr (Attr [] "Right")) :->:
    None [Attr Attr [("Right",0)] "False",Attr Attr [("Right",0)] "True"]
]
-}
typeLogic :: forall (d :: Type). (DeepGeneric d) => Formula d
typeLogic = All . sumLogic $ toConstructors (Proxy @d)
  where
    sumLogic :: Vector Constructor -> Vector (Formula d)
    sumLogic cs =
      -- Only one of the top-level constructors can be selected
      ExactlyOne (subVars cs)
        `Vector.cons`
        -- apply 'prodLogic' to all the fields
        Vector.concatMap prodLogic cs

    prodLogic :: Constructor -> Vector (Formula d)
    prodLogic (Constructor (ConsInfo cn _) cs) =
      -- for each present constructor, one of the constructors of each of its
      -- fields can be selected
      ( rootVar cn
          :->: (All . Vector.imap (\i -> ExactlyOne . pushedSubvars cn i) $ cs)
      )
        `Vector.cons`
        -- for each absent constructor, none of the constructors of any of its
        -- fields can be selected
        ( ( Not (rootVar cn)
              :->: (None . join . Vector.imap (pushedSubvars cn) $ cs)
          )
            -- recurse
            `Vector.cons` join
              ( Vector.imap
                  ( \i ->
                      fmap (fmap $ pushVR cn i)
                        . Vector.concatMap prodLogic
                  )
                  cs
              )
        )

    pushedSubvars ::
      SOP.ConstructorName -> Int -> Vector Constructor -> Vector (Formula d)
    pushedSubvars cn i = fmap (fmap (pushVR cn i)) . subVars

    subVars :: Vector Constructor -> Vector (Formula d)
    subVars = fmap (rootVar . consName . constructorInfo)

rootVar :: forall (d :: Type). SOP.ConstructorName -> Formula d
rootVar = Var . rootVarRep

{- |
The full set of valid attributes for a type.

Call using a type application.

= Examples

>>> allAttributes (Proxy @())
fromList [([],"()")]

>>> allAttributes (Proxy @Bool)
fromList
  [ ([],"False")
  , ([],"True")
  ]

>>> allAttributes (Proxy @(Bool,Bool))
fromList
  [ ([],"(,)")
  , ([("(,)",0)],"False")
  , ([("(,)",0)],"True")
  , ([("(,)",1)],"False")
  , ([("(,)",1)],"True")
  ]

>>> allAttributes (Proxy @(Either Bool Bool))
fromList
  [ ([],"Left")
  , ([],"Right")
  , ([("Left",0)],"False")
  , ([("Left",0)],"True")
  , ([("Right",0)],"False")
  , ([("Right",0)],"True")
  ]

>>> allAttributes (Proxy @(Either Bool (Bool,Bool)))
fromList
  [ ([],"Left")
  , ([],"Right")
  , ([("Left",0)],"False")
  , ([("Left",0)],"True")
  , ([("Right",0)],"(,)")
  , ([("Right",0),("(,)",0)],"False")
  , ([("Right",0),("(,)",0)],"True")
  , ([("Right",0),("(,)",1)],"False")
  , ([("Right",0),("(,)",1)],"True")
  ]

>>> allAttributes (Proxy @(Bool, First Bool))
fromList
  [ ([],"(,)")
  , ([("(,)",0)],"False")
  , ([("(,)",0)],"True")
  , ([("(,)",1)],"First")
  , ([("(,)",1),("First","getFirst")],"False")
  , ([("(,)",1),("First","getFirst")],"True")
  ]
-}
allAttributes ::
  forall (d :: Type).
  (DeepGeneric d) =>
  Proxy d ->
  Set (Vector (SOP.ConstructorName, FieldSelector), SOP.ConstructorName)
allAttributes p =
  Set.map
    ((\Attr {attrPath, attrConstr} -> (attrPath, attrConstr)) . attrIntToFS)
    (allAttributes' p)

allAttributes' ::
  forall (d :: Type). (DeepGeneric d) => Proxy d -> Set (Attribute Int d)
allAttributes' = Set.unions . fmap constructorAttributes . toConstructors
  where
    constructorAttributes :: Constructor -> Set (Attribute Int d)
    constructorAttributes =
      foldConstructor
        ( \(ConsInfo cn _) flds ->
            Set.singleton (rootVarRep cn)
              <> Set.unions
                (Vector.imap (\i -> Set.map (pushVR cn i) . Set.unions) flds)
        )

{- | Match against descriptions containing the given attribute.

An attribute represents a single constructor nested somewhere within an ADT,
and hence a single property captured by a description type. Separate fields of
the same type are represented by different attributes. The 'allAttributes'
function can be used to query the possible attributes of a type.

'attr' takes two arguments:

* the \'path\' to locate the attribute within the ADT, as a list of pairs of the
containing field, and a field selector of type 'FieldSelector'.

    The 'FieldSelector' type is a little bit magic, see its docs for more
    details.

* The name of the constructor representing the attribute.

See also the examples for 'allAttributes' to see what an attribute looks like.
-}
attr ::
  forall (d :: Type).
  (DeepGeneric d) =>
  -- | The \'path\' to the attribute.
  Vector (SOP.ConstructorName, FieldSelector) ->
  -- | The name of the constructor corresponding to the attribute.
  SOP.ConstructorName ->
  Formula d
attr p = Var . attrFSToInt . Attr p

transformAttr ::
  forall (d :: Type) (i :: Type) (j :: Type).
  (DeepGeneric d) =>
  (Maybe (Vector SOP.FieldName) -> i -> j) ->
  (Maybe (Vector SOP.FieldName) -> i -> Int) ->
  Attribute i d ->
  Attribute j d
transformAttr trans idx Attr {attrPath, attrConstr} =
  Attr (evalState (mapM act attrPath) (toConstructors $ Proxy @d)) attrConstr
  where
    act ::
      (SOP.ConstructorName, i) ->
      State (Vector Constructor) (SOP.ConstructorName, j)
    act (cn, i) = do
      con <- gets (findConstructor cn)
      let lab = consFields . constructorInfo $ con
      put $ subConstructors con Vector.! idx lab i
      return (cn, trans lab i)

attrFSToInt ::
  forall (d :: Type).
  (DeepGeneric d) =>
  Attribute FieldSelector d ->
  Attribute Int d
attrFSToInt = transformAttr resolveFS resolveFS
  where
    resolveFS :: Maybe (Vector SOP.FieldName) -> FieldSelector -> Int
    resolveFS _ (Index i) = i
    resolveFS con (RecordField fld) = resolveField con fld

    resolveField :: Maybe (Vector SOP.FieldName) -> SOP.FieldName -> Int
    resolveField con fld = fromJust $ Vector.elemIndex fld =<< con

attrIntToFS ::
  forall (d :: Type).
  (DeepGeneric d) =>
  Attribute Int d ->
  Attribute FieldSelector d
attrIntToFS = transformAttr index (const id)
  where
    index :: Maybe (Vector SOP.FieldName) -> Int -> FieldSelector
    index cons i =
      case cons of
        Nothing -> Index i
        Just flds -> RecordField (flds Vector.! i)

findConstructor :: SOP.ConstructorName -> Vector Constructor -> Constructor
findConstructor con =
  Vector.head . Vector.filter ((== con) . consName . constructorInfo)

logic :: forall (d :: Type) (a :: Type). (Description d a) => Formula d
logic = typeLogic :&&: refineDescription

enumerateScenariosWhere ::
  forall (d :: Type) (a :: Type).
  (Description d a) =>
  Formula d ->
  Set (Set (Attribute Int d))
enumerateScenariosWhere holds = enumerateSolutions $ logic :&&: holds

scenarios ::
  forall (d :: Type) (a :: Type).
  (Description d a) =>
  Set (Set (Attribute Int d))
scenarios = enumerateScenariosWhere Yes

{- |
Whether a description satisfies a formula.

Use with 'Apropos.Runner.passIf' to create a predicate to pass to a runner from
a 'Formula'.
-}
satisfies :: forall (d :: Type). (DeepGeneric d) => Formula d -> (d -> Bool)
satisfies f s =
  satisfiable $ f :&&: All (setToVars set) :&&: None (setToVars unset)
  where
    set :: Set (Attribute Int d)
    set = descriptionToVariables s

    unset :: Set (Attribute Int d)
    unset = Set.difference (allAttributes' $ Proxy @d) set

    setToVars :: Set (Attribute Int d) -> Vector (Formula d)
    setToVars = fmap Var . Vector.fromList . Set.toList

infixr 6 :&&:
infixr 5 :||:
infixr 4 :++:
infixr 2 :->:
infix 1 :<->:

{- |
  Logical expressions for matching description types.
-}
type Formula attr = Formula' (Attribute Int attr)
type Formula' :: Type -> Type
data Formula' attr
  = Var attr
  | Yes
  | No
  | Not (Formula' attr)
  | Formula' attr :&&: Formula' attr
  | Formula' attr :||: Formula' attr
  | Formula' attr :++: Formula' attr
  | Formula' attr :->: Formula' attr
  | Formula' attr :<->: Formula' attr
  | All (Vector (Formula' attr))
  | Some (Vector (Formula' attr))
  | None (Vector (Formula' attr))
  | ExactlyOne (Vector (Formula' attr))
  | AtMostOne (Vector (Formula' attr))
  deriving stock (Generic,Functor,Eq,Ord)

translateToSAT ::
  forall (attr :: Type). Formula attr -> MiniSAT.Formula (Attribute Int attr)
translateToSAT (Var var) = MiniSAT.Var var
translateToSAT Yes = MiniSAT.Yes
translateToSAT No = MiniSAT.No
translateToSAT (Not c) = MiniSAT.Not (translateToSAT c)
translateToSAT (a :&&: b) = translateToSAT a MiniSAT.:&&: translateToSAT b
translateToSAT (a :||: b) = translateToSAT a MiniSAT.:||: translateToSAT b
translateToSAT (a :++: b) = translateToSAT a MiniSAT.:++: translateToSAT b
translateToSAT (a :->: b) = translateToSAT a MiniSAT.:->: translateToSAT b
translateToSAT (a :<->: b) = translateToSAT a MiniSAT.:<->: translateToSAT b
translateToSAT (All cs) = MiniSAT.All . Vector.toList $ translateToSAT <$> cs
translateToSAT (Some cs) = MiniSAT.Some . Vector.toList $ translateToSAT <$> cs
translateToSAT (None cs) = MiniSAT.None . Vector.toList $ translateToSAT <$> cs
translateToSAT (ExactlyOne cs) =
  MiniSAT.ExactlyOne . Vector.toList $ translateToSAT <$> cs
translateToSAT (AtMostOne cs) =
  MiniSAT.AtMostOne . Vector.toList $ translateToSAT <$> cs

-- instance (Show attr, DeepGeneric attr) => Show (Formula attr) where
--   show a = show (translateToSAT a)

satisfiable :: forall (attr :: Type). Formula attr -> Bool
satisfiable = MiniSAT.satisfiable . translateToSAT

solveAll ::
  forall (attr :: Type). Formula attr -> Vector (Map (Attribute Int attr) Bool)
solveAll = Vector.fromList . MiniSAT.solve_all . translateToSAT

enumerateSolutions ::
  forall (attr :: Type). Formula attr -> Set (Set (Attribute Int attr))
enumerateSolutions f =
  Set.fromList . Vector.toList $ Map.keysSet . Map.filter id <$> solveAll f
