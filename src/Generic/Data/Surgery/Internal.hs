{-# LANGUAGE
    AllowAmbiguousTypes,
    BangPatterns,
    ConstraintKinds,
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    FlexibleInstances,
    LambdaCase,
    MultiParamTypeClasses,
    PolyKinds,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    TypeInType,
    UndecidableInstances,
    UndecidableSuperClasses #-}

-- | Operate on data types: insert\/modify\/delete fields and constructors.

module Generic.Data.Surgery.Internal where

import Control.Monad ((<=<))
import Data.Bifunctor (first, second)
import Data.Coerce
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits

import Fcf
  ( Exp, Eval, If, Pure, Pure2, Bimap, Uncurry
  , type (@@), type (=<<), type (<=<), type (<$>)
  )

import Generic.Data (MetaOf, MetaConsName)
import Generic.Data.Internal.Compat (Div)
import Generic.Data.Internal.Data (Data(Data,unData))
import Generic.Data.Internal.Meta (UnM1)
import Generic.Data.Internal.Utils (coerce', absurd1)

-- | /A sterile Operating Room, where generic data comes to be altered./
--
-- Generic representation in a simplified shape @l@ at the type level
-- (reusing the constructors from "GHC.Generics" for convenience).
-- This representation makes it easy to modify fields and constructors.
--
-- We may also refer to the representation @l@ as a "row" of constructors,
-- if it represents a sum type, otherwise it is a "row" of unnamed fields or
-- record fields for single-constructor types.
--
-- @x@ corresponds to the last parameter of 'Rep', and is currently ignored by
-- this module (no support for 'Generic1').
--
-- === General sketch
--
-- >
-- >                toOR                       surgeries                    fromOR'
-- > data MyType  -------->  OR (Rep MyType)  ---------->  OR alteredRep  --------->  Data alteredRep
-- >                                                                                        |
-- >                                                                                        | myGenericFun :: Generic a => a -> a
-- >                fromOR                     surgeries                    toOR'           v
-- > data MyType  <--------  OR (Rep MyType)  <----------  OR alteredRep  <---------  Data alteredRep
-- >
--
-- If instead @myGenericFun@ is only a consumer of @a@ (resp. producer),
-- then you only need the top half of the diagram (resp. bottom half).
-- For example, in aeson:
-- @genericToJSON@ (consumer), @genericParseJSON@ (producer).
newtype OR (l :: k -> Type) (x :: k) = OR { unOR :: l x }

-- | /Move fresh data to the Operating Room, where surgeries can be applied./
--
-- Convert a generic type to a generic representation.
--
-- When inserting or removing fields, there may be a mismatch with strict/unpacked fields.
-- To work around this, you can switch to 'toORLazy', if your operations don't care about
-- dealing with a normalized 'Rep' (in which all the strictness annotations have been
-- replaced with lazy defaults).
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- a :: 'Type'       -- Generic type
-- l :: k -> 'Type'  -- Generic representation (simplified)
-- x :: k          -- Ignored
-- @
--
-- ==== Functional dependencies
--
-- @
-- a -> l
-- @
toOR :: forall a l x. (Generic a, ToORRep a l) => a -> OR l x
toOR = OR . gLinearize . from

-- | /Move normalized data to the Operating Room, where surgeries can be applied./
--
-- Convert a generic type to a generic representation, in which all the strictness
-- annotations have been normalized to lazy defaults.
--
-- This variant is useful when one needs to operate on fields whose 'Rep' has different
-- strictness annotations than the ones used by 'DefaultMetaSel'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- a :: 'Type'       -- Generic type
-- l :: k -> 'Type'  -- Generic representation (simplified and normalized)
-- x :: k          -- Ignored
-- @
--
-- ==== Functional dependencies
--
-- @
-- a -> l
-- @
toORLazy :: forall a l x. (Generic a, ToORRepLazy a l) => a -> OR l x
toORLazy = OR . gLinearize @(Arborify l) . coerce' . from

-- | /Move altered data out of the Operating Room, to be consumed by/
-- /some generic function./
--
-- Convert a generic representation to a \"synthetic\" type that behaves
-- like a generic type.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- f :: k -> 'Type'  -- 'Generic' representation (proper)
-- l :: k -> 'Type'  -- Generic representation (simplified)
-- x :: k          -- Ignored
-- @
--
-- ==== Functional dependencies
--
-- @
-- f -> l
-- l -> f
-- @
--
-- ==== Implementation details
--
-- The synthesized representation is made of balanced binary trees,
-- corresponding closely to what GHC would generate for an actual data type.
--
-- That structure assumed by at least one piece of code out there (@aeson@).
fromOR' :: forall f l x. FromOR f l => OR l x -> Data f x
fromOR' = Data . gArborify . unOR

-- | /Move altered data, produced by some generic function, to the operating/
-- /room./
--
-- The inverse of 'fromOR''.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- f :: k -> 'Type'  -- 'Generic' representation (proper)
-- l :: k -> 'Type'  -- Generic representation (simplified)
-- x :: k          -- Ignored
-- @
--
-- ==== Functional dependencies
--
-- @
-- f -> l
-- l -> f
-- @
toOR' :: forall f l x. ToOR f l => Data f x -> OR l x
toOR' = OR . gLinearize . unData

-- | /Move restored data out of the Operating Room and back to the real/
-- /world./
--
-- The inverse of 'toOR'.
--
-- It may be useful to annotate the output type of 'fromOR',
-- since the rest of the type depends on it and the only way to infer it
-- otherwise is from the context. The following annotations are possible:
--
-- @
-- 'fromOR' :: 'OROf' a -> a
-- 'fromOR' \@a  -- with TypeApplications
-- @
--
-- When inserting or removing fields, there may be a mismatch with strict/unpacked fields.
-- To work around this, you can switch to 'fromORLazy', if your operations don't care
-- about dealing with a normalized 'Rep' (in which all the strictness annotations have
-- been replaced with lazy defaults).
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- a :: 'Type'       -- Generic type
-- l :: k -> 'Type'  -- Generic representation (simplified)
-- x :: k          -- Ignored
-- @
--
-- ==== Functional dependencies
--
-- @
-- a -> l
-- @
fromOR :: forall a l x. (Generic a, FromORRep a l) => OR l x -> a
fromOR = to . gArborify . unOR

-- | /Move normalized data out of the Operating Room and back to the real/
-- /world./
--
-- The inverse of 'toORLazy'.
--
-- It may be useful to annotate the output type of 'fromORLazy',
-- since the rest of the type depends on it and the only way to infer it
-- otherwise is from the context. The following annotations are possible:
--
-- @
-- 'fromORLazy' :: 'OROfLazy' a -> a
-- 'fromORLazy' \@a  -- with TypeApplications
-- @
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- a :: 'Type'       -- Generic type
-- l :: k -> 'Type'  -- Generic representation (simplified and normalized)
-- x :: k          -- Ignored
-- @
--
-- ==== Functional dependencies
--
-- @
-- a -> l
-- @
fromORLazy :: forall a l x. (Generic a, FromORRepLazy a l) => OR l x -> a
fromORLazy = to . coerce' . gArborify @(Lazify (Rep a)) . unOR

-- | The simplified generic representation type of type @a@,
-- that 'toOR' and 'fromOR' convert to and from.
type OROf a = OR (Linearize (Rep a)) ()

-- | The simplified and normalized generic representation type of type @a@,
-- that 'toORLazy' and 'fromORLazy' convert to and from.
type OROfLazy a = OR (Linearize (Lazify (Rep a))) ()

-- | This constraint means that @a@ is convertible /to/ its simplified
-- generic representation. Implies @'OROf' a ~ 'OR' l ()@.
type   ToORRep a l =   ToOR (Rep a) l

-- | This constraint means that @a@ is convertible /from/ its simplified
-- generic representation. Implies @'OROf' a ~ 'OR' l ()@.
type FromORRep a l = FromOR (Rep a) l

-- | Similar to 'ToORRep', but as a constraint on the standard
-- generic representation of @a@ directly, @f ~ 'Rep' a@.
type   ToOR f l = (GLinearize f, Linearize f ~ l, f ~ Arborify l)

-- | Similar to 'FromORRep', but as a constraint on the standard
-- generic representation of @a@ directly, @f ~ 'Rep' a@.
type FromOR f l = (GArborify  f, Linearize f ~ l, f ~ Arborify l)

-- | This constraint means that @a@ is convertible /to/ its simplified
-- and normalized generic representation (i.e., with all its strictness
-- annotations normalized to lazy defaults).
-- Implies @'OROfLazy' a ~ 'OR' l ()@.
type   ToORRepLazy a l =   ToORLazy (Rep a) l

-- | This constraint means that @a@ is convertible /from/ its simplified
-- and normalized generic representation (i.e., with all its strictness
-- annotations normalized to lazy defaults).
-- Implies @'OROfLazy' a ~ 'OR' l ()@.
type FromORRepLazy a l = FromORLazy (Rep a) l

-- | Similar to 'FromLazyORRep', but as a constraint on the standard
-- generic representation of @a@ directly, @f ~ 'Rep' a@.
type FromORLazy f l = (FromOR (Lazify f) l, Coercible (Arborify l) f)

-- | Similar to 'ToORRepLazy', but as a constraint on the standard
-- generic representation of @a@ directly, @f ~ 'Rep' a@.
type   ToORLazy f l = (ToOR (Lazify f) l, Coercible f (Arborify l))

--

-- | @'removeCField' \@n \@t@: remove the @n@-th field, of type @t@, in a
-- non-record single-constructor type.
--
-- Inverse of 'insertCField'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- n  :: 'Nat'        -- Field position
-- t  :: 'Type'       -- Field type
-- lt :: k -> 'Type'  -- Row with    field
-- l  :: k -> 'Type'  -- Row without field
-- x  :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- 'OR' lt x      -- Data with field
-- ->
-- (t, 'OR' l x)  -- Field value × Data without field
-- @
--
-- ==== Functional dependencies
--
-- @
-- n lt  -> t l
-- n t l -> lt
-- @
removeCField
  :: forall    n t lt l x
  .  RmvCField n t lt l
  => OR lt x -> (t, OR l x)
removeCField (OR a) = OR <$> gRemoveField @n a

-- | @'removeRField' \@\"fdName\" \@n \@t@: remove the field @fdName@
-- at position @n@ of type @t@ in a record type.
--
-- Inverse of 'insertRField'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- fd :: 'Symbol'     -- Field name
-- n  :: 'Nat'        -- Field position
-- t  :: 'Type'       -- Field type
-- lt :: k -> 'Type'  -- Row with    field
-- l  :: k -> 'Type'  -- Row without field
-- x  :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- 'OR' lt x      -- Data with field
-- ->
-- (t, 'OR' l x)  -- Field value × Data without field
-- @
--
-- ==== Functional dependencies
--
-- @
-- fd lt    -> n  t l
-- n  lt    -> fd t l
-- fd n t l -> lt
-- @
removeRField
  :: forall    fd n t lt l x
  .  RmvRField fd n t lt l
  => OR lt x -> (t, OR l x)
removeRField (OR a) = OR <$> gRemoveField @n a

-- | @'insertCField' \@n \@t@: insert a field of type @t@
-- at position @n@ in a non-record single-constructor type.
--
-- Inverse of 'removeCField'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- n  :: 'Nat'        -- Field position
-- t  :: 'Type'       -- Field type
-- lt :: k -> 'Type'  -- Row with    field
-- l  :: k -> 'Type'  -- Row without field
-- x  :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- (t, 'OR' l x)  -- Field value × Data without field
-- ->
-- 'OR' lt x      -- Data with field
-- @
--
-- ==== Functional dependencies
--
-- @
-- n lt  -> t l
-- n t l -> lt
-- @
insertCField
  :: forall    n t lt l x
  .  InsCField n t lt l
  => (t, OR l x) -> OR lt x
insertCField = uncurry (insertCField' @n)

-- | Curried 'insertCField'.
insertCField'
  :: forall    n t lt l x
  .  InsCField n t lt l
  => t -> OR l x -> OR lt x
insertCField' z (OR a) = OR (gInsertField @n z a)

-- | @'insertRField' \@\"fdName\" \@n \@t@: insert a field
-- named @fdName@ of type @t@ at position @n@ in a record type.
--
-- Inverse of 'removeRField'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- fd :: 'Symbol'     -- Field name
-- n  :: 'Nat'        -- Field position
-- t  :: 'Type'       -- Field type
-- lt :: k -> 'Type'  -- Row with    field
-- l  :: k -> 'Type'  -- Row without field
-- x  :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- (t, 'OR' l x)  -- Field value × Data without field
-- ->
-- 'OR' lt x      -- Data with field
-- @
--
-- ==== Functional dependencies
--
-- @
-- fd lt    -> n  t l
-- n  lt    -> fd t l
-- fd n t l -> lt
-- @
insertRField
  :: forall    fd n t lt l x
  .  InsRField fd n t lt l
  => (t, OR l x) -> OR lt x
insertRField = uncurry (insertRField' @fd)

-- | Curried 'insertRField'.
insertRField'
  :: forall    fd n t lt l x
  .  InsRField fd n t lt l
  => t -> OR l x -> OR lt x
insertRField' z (OR a) = OR (gInsertField @n z a)

-- | @'modifyCField' \@n \@t \@t'@: modify the field at position @n@ in a
-- non-record via a function @f :: t -> t'@ (changing the type of the field).
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- n   :: 'Nat'        -- Field position
-- t   :: 'Type'       -- Initial field type
-- t'  :: 'Type'       -- Final   field type
-- lt  :: k -> 'Type'  -- Row with initial field
-- lt' :: k -> 'Type'  -- Row with final   field
-- l   :: k -> 'Type'  -- Row without field
-- x   :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- (t -> t')  -- Field modification
-- ->
-- 'OR' lt  x   -- Data with field t
-- ->
-- 'OR' lt' x   -- Data with field t'
-- @
--
-- ==== Functional dependencies
--
-- @
-- n lt   -> t  l
-- n lt'  -> t' l
-- n t  l -> lt
-- n t' l -> lt'
-- @
modifyCField
  :: forall n t t' lt lt' l x
  .  ModCField n t t' lt lt' l
  => (t -> t') -> OR lt x -> OR lt' x
modifyCField f = insertCField @n @t' . first f . removeCField @n @t

-- | @'modifyRField' \@\"fdName\" \@n \@t \@t'@: modify the field
-- @fdName@ at position @n@ in a record via a function @f :: t -> t'@
-- (changing the type of the field).
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- fd  :: 'Symbol'     -- Field name
-- n   :: 'Nat'        -- Field position
-- t   :: 'Type'       -- Initial field type
-- t'  :: 'Type'       -- Final   field type
-- lt  :: k -> 'Type'  -- Row with initial field
-- lt' :: k -> 'Type'  -- Row with final   field
-- l   :: k -> 'Type'  -- Row without field
-- x   :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- (t -> t')  -- Field modification
-- ->
-- 'OR' lt  x   -- Data with field t
-- ->
-- 'OR' lt' x   -- Data with field t'
-- @
--
-- ==== Functional dependencies
--
-- @
-- fd lt     -> n  t  l
-- fd lt'    -> n  t' l
-- n  lt     -> fd t  l
-- n  lt'    -> fd t' l
-- fd n t  l -> lt
-- fd n t' l -> lt'
-- @
modifyRField
  :: forall fd n t t' lt lt' l x
  .  ModRField fd n t t' lt lt' l
  => (t -> t') -> OR lt x -> OR lt' x
modifyRField f = insertRField @fd @n @t' . first f . removeRField @fd @n @t

-- | @'removeConstr' \@\"C\" \@n \@t@: remove the @n@-th constructor, named @C@,
-- with contents isomorphic to the tuple @t@.
--
-- Inverse of 'insertConstr'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- c   :: 'Symbol'     -- Constructor name
-- t   :: 'Type'       -- Tuple type to hold c's contents
-- n   :: 'Nat'        -- Constructor position
-- lc  :: k -> 'Type'  -- Row with    constructor
-- l   :: k -> 'Type'  -- Row without constructor
-- l_t :: k -> 'Type'  -- Field row of constructor c
-- x   :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- 'OR' lc x            -- Data with constructor
-- ->
-- Either t ('OR' l x)  -- Constructor (as a tuple) | Data without constructor
-- @
--
-- ==== Functional dependencies
--
-- @
-- c lc      -> n l l_t
-- n lc      -> c l l_t
-- c n l l_t -> lc
-- @
--
-- Note that there is no dependency to determine @t@.
removeConstr
  :: forall    c n t lc l x
  .  RmvConstr c n t lc l
  => OR lc x -> Either t (OR l x)
removeConstr (OR a) = second OR (gRemoveConstr @n a)

-- | A variant of 'removeConstr' that can infer the tuple type @t@ to hold
-- the contents of the removed constructor.
--
-- See 'removeConstr'.
--
-- === __Details__
--
-- ==== Extra functional dependency
--
-- @
-- l_t -> t
-- @
removeConstrT
  :: forall     c n t lc l x
  .  RmvConstrT c n t lc l
  => OR lc x -> Either t (OR l x)
removeConstrT = removeConstr @c @n @t

-- | @'insertConstr' \@\"C\" \@n \@t@: insert a constructor @C@ at position @n@
-- with contents isomorphic to the tuple @t@.
--
-- Inverse of 'removeConstr'.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- c   :: 'Symbol'     -- Constructor name
-- t   :: 'Type'       -- Tuple type to hold c's contents
-- n   :: 'Nat'        -- Constructor position
-- lc  :: k -> 'Type'  -- Row with    constructor
-- l   :: k -> 'Type'  -- Row without constructor
-- l_t :: k -> 'Type'  -- Field row of constructor c
-- x   :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- Either t ('OR' l x)  -- Constructor (as a tuple) | Data without constructor
-- ->
-- 'OR' lc x            -- Data with constructor
-- @
--
-- ==== Functional dependencies
--
-- @
-- c lc      -> n l l_t
-- n lc      -> c l l_t
-- c n l l_t -> lc
-- @
--
-- Note that there is no dependency to determine @t@.
insertConstr
  :: forall    c n t lc l x
  .  InsConstr c n t lc l
  => Either t (OR l x) -> OR lc x
insertConstr z = OR (gInsertConstr @n (second unOR z))

-- | A variant of 'insertConstr' that can infer the tuple type @t@ to hold
-- the contents of the inserted constructor.
--
-- See 'insertConstr'.
--
-- === __Details__
--
-- ==== Extra functional dependency
--
-- @
-- l_t -> t
-- @
insertConstrT
  :: forall     c n t lc l x
  .  InsConstrT c n t lc l
  => Either t (OR l x) -> OR lc x
insertConstrT = insertConstr @c @n @t

-- | @'modifyConstr' \@\"C\" \@n \@t \@t'@: modify the @n@-th constructor,
-- named @C@, with contents isomorphic to the tuple @t@, to another tuple @t'@.
--
-- === __Details__
--
-- ==== Type parameters
--
-- @
-- c    :: 'Symbol'     -- Constructor name
-- t    :: 'Type'       -- Tuple type to hold c's initial contents
-- t'   :: 'Type'       -- Tuple type to hold c's final   contents
-- n    :: 'Nat'        -- Constructor position
-- lc   :: k -> 'Type'  -- Row with initial constructor
-- lc'  :: k -> 'Type'  -- Row with final   constructor
-- l    :: k -> 'Type'  -- Row without constructor
-- l_t  :: k -> 'Type'  -- Initial field row of constructor c
-- l_t' :: k -> 'Type'  -- Final   field row of constructor c
-- x    :: k          -- Ignored
-- @
--
-- ==== Signature
--
-- @
-- (t -> t')  -- Constructor modification
-- ->
-- 'OR' lc  x   -- Data with initial constructor
-- ->
-- 'OR' lc' x   -- Data with final   constructor
-- @
--
-- ==== Functional dependencies
--
-- @
-- c lc       -> n l l_t
-- c lc'      -> n l l_t'
-- n lc       -> c l l_t
-- n lc'      -> c l l_t'
-- c n l l_t  -> lc
-- c n l l_t' -> lc'
-- @
--
-- Note that there is no dependency to determine @t@ and @t'@.
modifyConstr
  :: forall    c n t t' lc lc' l x
  .  ModConstr c n t t' lc lc' l
  => (t -> t') -> OR lc x -> OR lc' x
modifyConstr f = insertConstr @c @n @t' . first f . removeConstr @c @n @t

-- | A variant of 'modifyConstr' that can infer the tuple types @t@ and @t'@ to
-- hold the contents of the inserted constructor.
--
-- See 'modifyConstr'.
--
-- === __Details__
--
-- ==== Extra functional dependencies
--
-- @
-- l_t  -> t
-- l_t' -> t'
-- @
modifyConstrT
  :: forall     c n t t' lc lc' l x
  .  ModConstrT c n t t' lc lc' l
  => (t -> t') -> OR lc x -> OR lc' x
modifyConstrT = modifyConstr @c @n @t @t'

--

-- | This constraint means that the (unnamed) field row @lt@ contains
-- a field of type @t@ at position @n@, and removing it yields row @l@.
type RmvCField n t lt l =
  ( GRemoveField n t lt l
  , CFieldSurgery n t lt l
  )

-- | This constraint means that the record field row @lt@ contains a field of
-- type @t@ named @fd@ at position @n@, and removing it yields row @l@.
type RmvRField fd n t lt l =
  ( GRemoveField n t lt l
  , RFieldSurgery fd n t lt l
  )

-- | This constraint means that inserting a field @t@ at position @n@ in the
-- (unnamed) field row @l@ yields row @lt@.
type InsCField n t lt l =
  ( GInsertField n t l lt
  , CFieldSurgery n t lt l
  )

-- | This constraint means that inserting a field @t@ named @fd@ at position
-- @n@ in the record field row @l@ yields row @lt@.
type InsRField fd n t lt l =
  ( GInsertField n t l lt
  , RFieldSurgery fd n t lt l
  )

-- | This constraint means that modifying a field @t@ to @t'@ at position @n@
-- in the (unnamed) field row @lt@ yields row @lt'@.
-- @l@ is the row of fields common to @lt@ and @lt'@.
type ModCField n t t' lt lt' l =
  ( RmvCField n t  lt  l
  , InsCField n t' lt' l
  )

-- | This constraint means that modifying a field @t@ named @fd@ at position @n@
-- to @t'@ in the record field row @lt@ yields row @lt'@.
-- @l@ is the row of fields common to @lt@ and @lt'@.
type ModRField fd n t t' lt lt' l =
  ( RmvRField fd n t  lt  l
  , InsRField fd n t' lt' l
  )

-- | This constraint means that the constructor row @lc@ contains a constructor
-- named @c@ at position @n@, and removing it from @lc@ yields row @l@.
-- Furthermore, constructor @c@ contains a field row @l_t@ compatible with the
-- tuple type @t@.
type RmvConstr c n t lc l =
  ( GRemoveConstr n t lc l
  , ConstrSurgery c n t lc l (Eval (ConstrAt n lc))
  )

-- | A variant of 'RmvConstr' allowing @t@ to be inferred.
type RmvConstrT c n t lc l =
  ( RmvConstr c n t lc l
  , IsTuple (Arity (Eval (ConstrAt n lc))) t
  )

-- | This constraint means that inserting a constructor @c@ at position @n@
-- in the constructor row @l@ yields row @lc@.
-- Furthermore, constructor @c@ contains a field row @l_t@ compatible with the
-- tuple type @t@.
type InsConstr c n (t :: Type) lc l =
  ( GInsertConstr n t l lc
  , ConstrSurgery c n t lc l (Eval (ConstrAt n lc))
  )

-- | A variant of 'InsConstr' allowing @t@ to be inferred.
type InsConstrT c n t lc l =
  ( InsConstr c n t lc l
  , IsTuple (Arity (Eval (ConstrAt n lc))) t
  )

-- | This constraint means that the constructor row @lc@ contains a constructor
-- named @c@ at position @n@ of type isomorphic to @t@, and modifying it to
-- @t'@ yields row @lc'@.
type ModConstr c n t t' lc lc' l =
  ( RmvConstr c n t  lc  l
  , InsConstr c n t' lc' l
  )

-- | A variant of 'ModConstr' allowing @t@ and @t'@ to be inferred.
type ModConstrT c n t t' lc lc' l =
  ( ModConstr c n t t' lc lc' l
  , IsTuple (Arity (Eval (ConstrAt n lc ))) t
  , IsTuple (Arity (Eval (ConstrAt n lc'))) t'
  )

type FieldSurgery n t lt l =
  ( t ~ Eval (FieldTypeAt n lt)
  , l ~ Eval (RemoveField n t lt)
  )

type CFieldSurgery n t lt l =
  ( lt ~ Eval (InsertField n 'Nothing t l)
  , FieldSurgery n t lt l
  )

type RFieldSurgery fd n t lt l =
  ( n ~ Eval (FieldIndex fd lt)
  , lt ~ Eval (InsertField n ('Just fd) t l)
  , FieldSurgery n t lt l
  )

type ConstrSurgery c n t lc l l_t =
  ( Generic t
  , MatchFields (Linearize (UnM1 (Rep t))) l_t
  , n ~ Eval (ConstrIndex c lc)
  , c ~ MetaConsName (MetaOf l_t)
  , lc ~ Eval (InsertUConstrAtL n l_t l)
  , l ~ Eval (RemoveUConstrAt_ n lc)
  )

--

type family   Linearize (f :: k -> Type) :: k -> Type
type instance Linearize (M1 D m f) = M1 D m (LinearizeSum f V1)
type instance Linearize (M1 C m f) = M1 C m (LinearizeProduct f U1)

type family   LinearizeSum (f :: k -> Type) (tl :: k -> Type) :: k -> Type
type instance LinearizeSum V1 tl = tl
type instance LinearizeSum (f :+: g) tl = LinearizeSum f (LinearizeSum g tl)
type instance LinearizeSum (M1 c m f) tl = M1 c m (LinearizeProduct f U1) :+: tl

type family   LinearizeProduct (f :: k -> Type) (tl :: k -> Type) :: k -> Type
type instance LinearizeProduct U1 tl = tl
type instance LinearizeProduct (f :*: g) tl = LinearizeProduct f (LinearizeProduct g tl)
type instance LinearizeProduct (M1 s m f) tl = M1 s m f :*: tl

class GLinearize f where
  gLinearize :: f x -> Linearize f x

instance GLinearizeSum f V1 => GLinearize (M1 D m f) where
  gLinearize (M1 a) = M1 (gLinearizeSum @_ @V1 (Left a))

instance GLinearizeProduct f U1 => GLinearize (M1 C m f) where
  gLinearize (M1 a) = M1 (gLinearizeProduct a U1)

class GLinearizeSum f tl where
  gLinearizeSum :: Either (f x) (tl x) -> LinearizeSum f tl x

instance GLinearizeSum V1 tl where
  gLinearizeSum (Left  v) = absurd1 v
  gLinearizeSum (Right c) = c

instance (GLinearizeSum g tl, GLinearizeSum f (LinearizeSum g tl))
  => GLinearizeSum (f :+: g) tl where
  gLinearizeSum (Left (L1 a)) = gLinearizeSum @_ @(LinearizeSum g tl) (Left a)
  gLinearizeSum (Left (R1 b)) = gLinearizeSum @f (Right (gLinearizeSum @g @tl (Left b)))
  gLinearizeSum (Right c) = gLinearizeSum @f (Right (gLinearizeSum @g (Right c)))

instance GLinearizeProduct f U1 => GLinearizeSum (M1 c m f) tl where
  gLinearizeSum (Left (M1 a)) = L1 (M1 (gLinearizeProduct a U1))
  gLinearizeSum (Right c) = R1 c

class GLinearizeProduct f tl where
  gLinearizeProduct :: f x -> tl x -> LinearizeProduct f tl x

instance GLinearizeProduct U1 tl where
  gLinearizeProduct _ = id

instance (GLinearizeProduct g tl, GLinearizeProduct f (LinearizeProduct g tl))
  => GLinearizeProduct (f :*: g) tl where
  gLinearizeProduct (a :*: b) = gLinearizeProduct a . gLinearizeProduct b

instance GLinearizeProduct (M1 s m f) tl where
  gLinearizeProduct = (:*:)

class GArborify f where
  gArborify :: Linearize f x -> f x

instance GArborifySum f V1 => GArborify (M1 D m f) where
  gArborify (M1 a) = case gArborifySum @_ @V1 a of
    Left a' -> M1 a'
    Right v -> absurd1 v

instance GArborifyProduct f U1 => GArborify (M1 C m f) where
  gArborify (M1 a) = M1 (fst (gArborifyProduct @_ @U1 a))

class GArborifySum f tl where
  gArborifySum :: LinearizeSum f tl x -> Either (f x) (tl x)

instance GArborifySum V1 tl where
  gArborifySum = Right

instance (GArborifySum g tl, GArborifySum f (LinearizeSum g tl))
  => GArborifySum (f :+: g) tl where
  gArborifySum = first R1 . gArborifySum <=< first L1 . gArborifySum

instance GArborifyProduct f U1 => GArborifySum (M1 c m f) tl where
  gArborifySum (L1 (M1 a)) = Left (M1 (fst (gArborifyProduct @_ @U1 a)))
  gArborifySum (R1 c) = Right c

class GArborifyProduct f tl where
  gArborifyProduct :: LinearizeProduct f tl x -> (f x, tl x)

instance GArborifyProduct U1 tl where
  gArborifyProduct c = (U1, c)

instance (GArborifyProduct g tl, GArborifyProduct f (LinearizeProduct g tl))
  => GArborifyProduct (f :*: g) tl where
  gArborifyProduct abc = (a :*: b, c) where
    (a, bc) = gArborifyProduct abc
    (b,  c) = gArborifyProduct  bc

instance GArborifyProduct (M1 s m f) tl where
  gArborifyProduct (a :*: c) = (a, c)

type family   Arborify (f :: k -> Type) :: k -> Type
type instance Arborify (M1 D m f) = M1 D m (Eval (ArborifySum (CoArity f) f))
type instance Arborify (M1 C m f) = M1 C m (Eval (ArborifyProduct (Arity f) f))

data ArborifySum (n :: Nat) (f :: k -> Type) :: Exp (k -> Type)
type instance Eval (ArborifySum n V1) = V1
type instance Eval (ArborifySum n (f :+: g)) =
  Eval (If (n == 1)
    (ArborifyProduct (Arity f) f)
    (Arborify' ArborifySum (:+:) n (Div n 2) f g))

data ArborifyProduct (n :: Nat) (f :: k -> Type) :: Exp (k -> Type)
type instance Eval (ArborifyProduct n (M1 C s f)) = M1 C s (Eval (ArborifyProduct n f))
type instance Eval (ArborifyProduct n U1) = U1
type instance Eval (ArborifyProduct n (f :*: g)) =
  Eval (If (n == 1)
    (Pure f)
    (Arborify' ArborifyProduct (:*:) n (Div n 2) f g))

-- let nDiv2 = Div n 2 in ...
type Arborify' arb op n nDiv2 f g =
   (   Uncurry (Pure2 op)
   <=< Bimap (arb nDiv2) (arb (n-nDiv2))
   <=< SplitAt nDiv2
   ) (op f g)

type family Lazify (f :: k -> Type) :: k -> Type
type instance Lazify (M1 i m f) = M1 i (LazifyMeta m) (Lazify f)
type instance Lazify (f :*: g) = Lazify f :*: Lazify g
type instance Lazify (f :+: g) = Lazify f :+: Lazify g
type instance Lazify (K1 i c) = K1 i c
type instance Lazify U1 = U1
type instance Lazify V1 = V1

type family LazifyMeta (m :: Meta) :: Meta
type instance LazifyMeta ('MetaData n m p nt) = 'MetaData n m p nt
type instance LazifyMeta ('MetaCons n f s) = 'MetaCons n f s
type instance LazifyMeta ('MetaSel mn su ss ds)
  = 'MetaSel mn 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

data SplitAt :: Nat -> (k -> Type) -> Exp (k -> Type, k -> Type)
type instance Eval (SplitAt n (f :+: g)) =
  Eval (If (n == 0)
    (Pure '(V1, f :+: g))
    (Bimap (Pure2 (:+:) f) Pure =<< SplitAt (n-1) g))
type instance Eval (SplitAt n (f :*: g)) =
  Eval (If (n == 0)
    (Pure '(U1, f :*: g))
    (Bimap (Pure2 (:*:) f) Pure =<< SplitAt (n-1) g))

-- * Surgeries

-- | Kind of surgeries: operations on generic representations of types.
--
-- Treat this as an abstract kind (don't pay attention to its definition).
--
-- === __Implementation details__
--
-- The name @Surgery@ got taken first by generic-data.
--
-- @k@ is the kind of the extra parameter reserved for @Generic1@,
-- which we just don't use.
type MajorSurgery k = MajorSurgery_ k

-- Whenever you see
--   data ... :: MajorSurgery k
-- mentally expand it to
--   data ... (f :: k -> Type) :: Exp (k -> Type)

-- | @Operate f s@. Apply a surgery @s@ to a generic representation @f@
-- (e.g., @f = 'Rep' a@ for some 'Generic' type @a@).
--
-- The first argument is the generic representation;
-- the second argument is the surgery, which typically has the more complex
-- syntax, which is why this reverse application order was chosen.
type Operate (f :: k -> Type) (s :: MajorSurgery k) = Operate_ f s

-- | Internal definition of 'MajorSurgery'.
type MajorSurgery_ k = (k -> Type) -> Exp (k -> Type)

-- | Internal definition of 'Operate'.
type Operate_ (f :: k -> Type) (s :: MajorSurgery k) = Arborify (OperateL (Linearize f) s)

-- | Apply a surgery @s@ to a linearized generic representation @l@.
type OperateL (l :: k -> Type) (s :: MajorSurgery k) = Eval (s l)

-- | Composition of surgeries (left-to-right).
--
-- === Note
--
-- Surgeries work on normalized representations, so 'Operate', which applies
-- a surgery to a generic representation, inserts normalization steps before
-- and after the surgery. This means that @'Operate' r (s1 ':>>' s2)@ is not quite
-- the same as @'Operate' ('Operate' r s1) s2@. Instead, the latter is
-- equivalent to @'Operate' r (s1 ':>>' 'Suture' ':>>' s2)@, where 'Suture'
-- inserts some intermediate normalization steps.
data (:>>) :: MajorSurgery k -> MajorSurgery k -> MajorSurgery k
type instance Eval ((s :>> t) l) = Eval (t (Eval (s l)))
-- Note: This is a specialization of @(>=>)@ in Fcf.

type instance PerformL l (s :>> t) = (PerformL l s, PerformL (Eval (s l)) t)

infixl 1 :>>

-- | The identity surgery: doesn't do anything.
data IdSurgery :: MajorSurgery k
type instance Eval (IdSurgery l) = l
type instance PerformL l IdSurgery = ()

-- | Use this if a patient ever needs to go out and back into the operating
-- room, when it's not just to undo the surgery up to that point.
data Suture :: MajorSurgery k
type instance Eval (Suture l) = Linearize (Arborify l)

-- Now we can compose surgeries into complex ones, we can relate the input and
-- output of a whole surgery.
--
-- We still need to augment this with run-time information to 'Perform' the
-- surgery at the term level.
--
-- We might also need to interpret surgeries backwards (this is not entirely
-- symmetrical, a "removal" contains less information than an "insertion").

type family PerformL (l :: k -> Type) (s :: MajorSurgery k) :: Constraint

-- | A constraint @Perform r s@ means that the surgery @s@ can be applied to
-- the generic representation @r@.
class    Perform_ r s => Perform (r :: k -> Type) (s :: MajorSurgery k)
instance Perform_ r s => Perform (r :: k -> Type) (s :: MajorSurgery k)

type Perform_ (r :: k -> Type) (s :: MajorSurgery k) =
  ( PerformL (Linearize r) s
  , ToOR r (Linearize r)
  , FromOR (Operate r s) (OperateL (Linearize r) s)
  )

data FieldTypeAt (n :: Nat) (f :: k -> Type) :: Exp Type
type instance Eval (FieldTypeAt n (M1 i c f)) = Eval (FieldTypeAt n f)
type instance Eval (FieldTypeAt n (f :+: V1)) = Eval (FieldTypeAt n f)
type instance Eval (FieldTypeAt n (f :*: g)) =
  Eval (If (n == 0) (Pure (FieldTypeOf f)) (FieldTypeAt (n-1) g))

type family   FieldTypeOf (f :: k -> Type) :: Type
type instance FieldTypeOf (M1 s m (K1 i a)) = a

data FieldNameAt (n :: Nat) (f :: k -> Type) :: Exp (Maybe Symbol)
type instance Eval (FieldNameAt n (M1 i c f)) = Eval (FieldNameAt n f)
type instance Eval (FieldNameAt n (f :+: V1)) = Eval (FieldNameAt n f)
type instance Eval (FieldNameAt n (f :*: g)) =
  Eval (If (n == 0) (FieldNameOf f) (FieldNameAt (n-1) g))

data FieldNameOf (f :: k -> Type) :: Exp (Maybe Symbol)
type instance Eval (FieldNameOf (M1 S ('MetaSel mn _ _ _) _)) = mn

data RemoveField (n :: Nat) (a :: Type) :: MajorSurgery k
type instance Eval (RemoveField n a f) = Eval (RemoveField_ n f)

-- | Like 'RemoveField' but without the explicit field type.
data RemoveField_ (n :: Nat) :: MajorSurgery k
type instance Eval (RemoveField_ n (M1 i m f)) = M1 i m (Eval (RemoveField_ n f))
type instance Eval (RemoveField_ n (f :+: V1)) = Eval (RemoveField_ n f) :+: V1
type instance Eval (RemoveField_ n (f :*: g)) =
  Eval (If (n == 0) (Pure g) ((:*:) f <$> RemoveField_ (n-1) g))

type instance PerformL lt (RemoveField n a) = PerformL lt (RemoveFieldAt n (FieldNameAt n @@ lt) a)

data RemoveFieldAt (n :: Nat) (fd :: Maybe Symbol) (a :: Type) :: MajorSurgery k
type instance PerformL lt (RemoveFieldAt n fd a) =
  PerformLRemoveFieldAt n fd a lt (Eval (RemoveField_ n lt))

type PerformLRemoveFieldAt_ n fd t lt l =
  ( GRemoveField n t lt l
  , t ~ Eval (FieldTypeAt n lt)
  , lt ~ Eval (InsertField n fd t l)
  )

class    PerformLRemoveFieldAt_ n fd t lt l => PerformLRemoveFieldAt n fd t lt l
instance PerformLRemoveFieldAt_ n fd t lt l => PerformLRemoveFieldAt n fd t lt l

data RemoveRField (fd :: Symbol) (a :: Type) :: MajorSurgery k
type instance Eval (RemoveRField fd a f) = Eval (RemoveField_ (Eval (FieldIndex fd f)) f)

type instance PerformL lt (RemoveRField fd a) =
  PerformL lt (RemoveFieldAt (FieldIndex fd @@ lt) ('Just fd) a)

type DefaultMetaSel field
  = 'MetaSel field 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

data InsertField (n :: Nat) (fd :: Maybe Symbol) (t :: Type) :: MajorSurgery k
type instance Eval (InsertField n fd t (M1 D m f)) = M1 D m (Eval (InsertField n fd t f))
type instance Eval (InsertField n fd t (M1 C m f)) = M1 C m (Eval (InsertField n fd t f))
type instance Eval (InsertField n fd t (f :+: V1)) = Eval (InsertField n fd t f) :+: V1
type instance Eval (InsertField n fd t (f :*: g)) =
  Eval (If (n == 0)
    (Pure (M1 S (DefaultMetaSel fd) (K1 R t) :*: (f :*: g)))
    ((:*:) f <$> InsertField (n-1) fd t g))
type instance Eval (InsertField 0 fd t U1) = M1 S (DefaultMetaSel fd) (K1 R t) :*: U1

type instance PerformL l (InsertField n fd t) = PerformLInsert n fd t l (Eval (InsertField n fd t l))

type PerformLInsert_ n fd t l tl =
  ( GInsertField n t l tl
  , l ~ Eval (RemoveField_ n tl)
  , tl ~ Eval (InsertField n fd t l)
  , CheckField n fd tl
  , t ~ Eval (FieldTypeAt n tl)
  )

class    PerformLInsert_ n fd t l tl => PerformLInsert n fd t l tl
instance PerformLInsert_ n fd t l tl => PerformLInsert n fd t l tl

type family CheckField (n :: Nat) (fd :: Maybe Symbol) (tl :: k -> Type) :: Constraint where
  CheckField n 'Nothing tl = ()
  CheckField n ('Just fd) tl = (n ~ Eval (FieldIndex fd tl))

data Succ :: Nat -> Exp Nat
type instance Eval (Succ n) = 1 + n

-- | Position of a record field
data FieldIndex (field :: Symbol) (f :: k -> Type) :: Exp Nat
type instance Eval (FieldIndex field (M1 D m f)) = Eval (FieldIndex field f)
type instance Eval (FieldIndex field (M1 C m f)) = Eval (FieldIndex field f)
type instance Eval (FieldIndex field (f :+: V1)) = Eval (FieldIndex field f)
type instance Eval (FieldIndex field (M1 S ('MetaSel ('Just field') su ss ds) f :*: g))
  = Eval (If (field == field') (Pure 0) (Succ =<< FieldIndex field g))

-- | Number of fields of a single constructor
type family   Arity (f :: k -> Type) :: Nat
type instance Arity (M1 d m f) = Arity f
type instance Arity (f :+: V1) = Arity f
type instance Arity (f :*: g) = Arity f + Arity g
type instance Arity (K1 i c) = 1
type instance Arity U1 = 0

-- | Number of constructors of a data type
type family   CoArity (f :: k -> Type) :: Nat
type instance CoArity (M1 D m f) = CoArity f
type instance CoArity (M1 C m f) = 1
type instance CoArity V1         = 0
type instance CoArity (f :+: g)  = CoArity f + CoArity g

class GRemoveField (n :: Nat) a f g where
  gRemoveField :: f x -> (a, g x)

instance GRemoveField n a f g => GRemoveField n a (M1 i c f) (M1 i c g) where
  gRemoveField (M1 a) = M1 <$> gRemoveField @n a

-- Only single-constructor types are supported for the moment.
instance GRemoveField n a f g => GRemoveField n a (f :+: V1) (g :+: V1) where
  gRemoveField (L1 a) = L1 <$> gRemoveField @n a
  gRemoveField (R1 v) = absurd1 v

instance GRemoveField 0 a (M1 s m (K1 i a) :*: f) f where
  gRemoveField (M1 (K1 t) :*: b) = (t, b)

instance {-# OVERLAPPABLE #-}
  ( (n == 0) ~ 'False
  , f0g ~ (f0 :*: g)
  , GRemoveField (n-1) a f g
  ) => GRemoveField n a (f0 :*: f) f0g where
  gRemoveField (a :*: b) = (a :*:) <$> gRemoveField @(n-1) b

class GInsertField (n :: Nat) a f g where
  gInsertField :: a -> f x -> g x

instance GInsertField n a f g => GInsertField n a (M1 i c f) (M1 i c g) where
  gInsertField t (M1 a) = M1 (gInsertField @n t a)

instance GInsertField n a f g => GInsertField n a (f :+: V1) (g :+: V1) where
  gInsertField t (L1 a) = L1 (gInsertField @n t a)
  gInsertField _ (R1 v) = absurd1 v

instance GInsertField 0 a f (M1 s m (K1 i a) :*: f) where
  gInsertField t ab = M1 (K1 t) :*: ab

instance {-# OVERLAPPABLE #-}
  ( (n == 0) ~ 'False
  , f0f ~ (f0 :*: f)
  , GInsertField (n-1) a f g
  ) => GInsertField n a f0f (f0 :*: g) where
  gInsertField t (a :*: b) = a :*: gInsertField @(n-1) t b

data ConstrAt (n :: Nat) (f :: k -> Type) :: Exp (k -> Type)
type instance Eval (ConstrAt n (M1 i m f)) = Eval (ConstrAt n f)
type instance Eval (ConstrAt n (f :+: g)) =
  Eval (If (n == 0) (Pure f) (ConstrAt (n-1) g))

data RemoveConstr (c :: Symbol) (t :: Type) :: MajorSurgery k
type instance Eval (RemoveConstr c t l) = Eval (RemoveConstrAt c (ConstrIndex c @@ l) t l)

type instance PerformL lc (RemoveConstr c t) = PerformLRemoveConstr lc c (ConstrIndex c @@ lc) t

type PerformLRemoveConstr lc c n (t :: Type) =
  PerformLRemoveConstrAt c n t (Eval (ConstrAt n lc)) lc (Eval (RemoveUConstrAt_ n lc))

type PerformLRemoveConstrAt_ c n t l_t lc l =
  ( GRemoveConstr n t lc l
  -- , l_t ~ Linearize (Arborify l_t)
  , c ~ MetaConsName (MetaOf l_t)
  , lc ~ Eval (InsertUConstrAtL n l_t l)
  , MatchFields (Linearize (UnM1 (Rep t))) l_t
  , Arity l_t ~ Arity (Linearize (UnM1 (Rep t)))
  )

class    PerformLRemoveConstrAt_ c n t l_t lc l => PerformLRemoveConstrAt c n (t :: Type) l_t lc l
instance PerformLRemoveConstrAt_ c n t l_t lc l => PerformLRemoveConstrAt c n (t :: Type) l_t lc l

data RemoveConstrAt (c :: Symbol) (n :: Nat) (t :: Type) :: MajorSurgery k
type instance Eval (RemoveConstrAt _ n t l) = Eval (RemoveUConstrAt n t l)

data RemoveUConstrAt (n :: Nat) (t :: Type) :: MajorSurgery k
type instance Eval (RemoveUConstrAt n _ l) = Eval (RemoveUConstrAt_ n l)

-- | Like 'RemoveConstr', but without the explicit constructor type.
data RemoveUConstrAt_ (n :: Nat) :: MajorSurgery k
type instance Eval (RemoveUConstrAt_ n (M1 i m f)) = M1 i m (Eval (RemoveUConstrAt_ n f))
type instance Eval (RemoveUConstrAt_ n (f :+: g)) =
  Eval (If (n == 0) (Pure g) ((:+:) f <$> RemoveUConstrAt_ (n-1) g))

-- | This is polymorphic to allow different ways of specifying the inserted constructor.
--
-- If @sym@ (the kind of the constructor name @c@) is:
--
-- - 'Symbol': treat it like a regular prefix constructor.
-- - TODO Infix constructors and their fixities.
--
-- @t@ must be a single-constructor type, then we reuse its generic
-- representation for the new constructor, only replacing its constructor name
-- with @c@.
data InsertConstrAt (c :: sym) (n :: Nat) (t :: ty) :: MajorSurgery k
type instance Eval (InsertConstrAt c n t l) = Eval (InsertUConstrAtL n (ConGraft c t) l)

type family ConGraft (c :: sym) (t :: ty) :: k -> Type
type instance ConGraft c (t :: Type) = RenameCon c (Linearize (UnM1 (Rep t)))

type family RenameCon (c :: sym) (t :: k -> Type) :: k -> Type
type instance RenameCon c (M1 C m f) = M1 C (RenameMeta c m) f

type family RenameMeta (c :: sym) (m :: Meta) :: Meta
type instance RenameMeta (s :: Symbol) ('MetaCons _ _ r) = 'MetaCons s 'PrefixI r

type instance PerformL l (InsertConstrAt c n t) = PerformLInsertConstrAt0 l c n t

type PerformLInsertConstrAt0 l c n t =
  PerformLInsertConstrAt c n t (ConGraft c t) l (Eval (InsertUConstrAtL n (ConGraft c t) l))

type PerformLInsertConstrAt_ c n t l_t l lc =
  ( GInsertConstr n t l lc
  , c ~ MetaConsName (MetaOf l_t)
  , n ~ (ConstrIndex c @@ lc)
  , l_t ~ (ConstrAt n @@ lc)
  , l ~ Eval (RemoveUConstrAt_ n lc)
  , MatchFields (Linearize (UnM1 (Rep t))) l_t
  )

class    PerformLInsertConstrAt_ c n t l_t l lc => PerformLInsertConstrAt c n t l_t l lc
instance PerformLInsertConstrAt_ c n t l_t l lc => PerformLInsertConstrAt c n t l_t l lc

data InsertUConstrAt (n :: Nat) (t :: Type) :: MajorSurgery k
type instance Eval (InsertUConstrAt n t l) = Eval (InsertUConstrAtL n (Linearize (UnM1 (Rep t))) l)

data InsertUConstrAtL (n :: Nat) (t :: k -> Type) :: MajorSurgery k
type instance Eval (InsertUConstrAtL n t (M1 i m f)) = M1 i m (Eval (InsertUConstrAtL n t f))
type instance Eval (InsertUConstrAtL n t (f :+: g)) =
  Eval (If (n == 0) (Pure (t :+: (f :+: g))) ((:+:) f <$> InsertUConstrAtL (n-1) t g))
type instance Eval (InsertUConstrAtL 0 t V1) = t :+: V1

data ConstrIndex (con :: Symbol) (f :: k -> Type) :: Exp Nat
type instance Eval (ConstrIndex con (M1 D m f)) = Eval (ConstrIndex con f)
type instance Eval (ConstrIndex con (M1 C ('MetaCons con' fx s) f :+: g)) =
  Eval (If (con == con') (Pure 0) (Succ =<< ConstrIndex con g))

class GRemoveConstr (n :: Nat) (t :: Type) f g where
  gRemoveConstr :: f x -> Either t (g x)

instance GRemoveConstr n t f g => GRemoveConstr n t (M1 i c f) (M1 i c g) where
  gRemoveConstr (M1 a) = M1 <$> gRemoveConstr @n a

type ConstrArborify t l =
  ( Generic t
  , Coercible (UnM1 (Rep t)) (Rep t)
  , GArborify (UnM1 (Rep t))
  , Coercible l (Linearize (UnM1 (Rep t)))
  )

constrArborify' :: forall t l x. ConstrArborify t l => l x -> t
constrArborify' = to @t @x . coerce (gArborify @(UnM1 (Rep t)) @x)

instance ConstrArborify t l => GRemoveConstr 0 t (l :+: f) f where
  gRemoveConstr (L1 a) = Left (constrArborify' a)
  gRemoveConstr (R1 b) = Right b

instance {-# OVERLAPPABLE #-}
  ( GRemoveConstr (n-1) t f g, (n == 0) ~ 'False
  , f0g ~ (f0 :+: g)
  ) => GRemoveConstr n t (f0 :+: f) f0g where
  gRemoveConstr (L1 a) = Right (L1 a)
  gRemoveConstr (R1 b) = R1 <$> gRemoveConstr @(n-1) b

class GInsertConstr (n :: Nat) (t :: Type) f g where
  gInsertConstr :: Either t (f x) -> g x

instance GInsertConstr n t f g => GInsertConstr n t (M1 i c f) (M1 i c g) where
  gInsertConstr = M1 . gInsertConstr @n . fmap unM1

type ConstrLinearize t l =
  ( Generic t
  , Coercible (Rep t) (UnM1 (Rep t))
  , GLinearize (UnM1 (Rep t))
  , Coercible (Linearize (UnM1 (Rep t))) l
  )

constrLinearize' :: forall t l x. ConstrLinearize t l => t -> l x
constrLinearize' = coerce (gLinearize @(UnM1 (Rep t)) @x) . from @t @x

instance ConstrLinearize t l => GInsertConstr 0 t f (l :+: f) where
  gInsertConstr (Left a) = L1 (constrLinearize' a)
  gInsertConstr (Right b) = R1 b

instance {-# OVERLAPPABLE #-}
  ( GInsertConstr (n-1) t f g, (n == 0) ~ 'False
  , f0f ~ (f0 :+: f)
  ) => GInsertConstr n t f0f (f0 :+: g) where
  gInsertConstr (Left a) = R1 (gInsertConstr @(n-1) @t @f @g (Left a))
  gInsertConstr (Right (L1 a)) = L1 a
  gInsertConstr (Right (R1 b)) = R1 (gInsertConstr @(n-1) @t @f @g (Right b))

-- | Equate two generic representations, but ignoring constructor and field metadata.
class MatchFields (f :: k -> Type) (g :: k -> Type)
instance (g ~ M1 D c g', MatchFields f' g') => MatchFields (M1 D c f') g
instance (g ~ M1 C ('MetaCons _x _y _z) g', MatchFields f' g') => MatchFields (M1 C c f') g
instance (g ~ M1 S ('MetaSel _w _x _y _z) g', MatchFields f' g') => MatchFields (M1 S c f') g
instance (g ~ (g1 :+: g2), MatchFields f1 g1, MatchFields f2 g2) => MatchFields (f1 :+: f2) g
instance (g ~ (g1 :*: g2), MatchFields f1 g1, MatchFields f2 g2) => MatchFields (f1 :*: f2) g
instance (g ~ K1 i a) => MatchFields (K1 i a) g
instance (g ~ U1) => MatchFields U1 g
instance (g ~ V1) => MatchFields V1 g

class IsTuple (n :: Nat) (t :: k)
instance (t ~ ())                    => IsTuple 0 t
instance (t ~ Identity a)            => IsTuple 1 t
instance (t ~ (a, b))                => IsTuple 2 t
instance (t ~ (a, b, c))             => IsTuple 3 t
instance (t ~ (a, b, c, d))          => IsTuple 4 t
instance (t ~ (a, b, c, d, e))       => IsTuple 5 t
instance (t ~ (a, b, c, d, e, f))    => IsTuple 6 t
instance (t ~ (a, b, c, d, e, f, g)) => IsTuple 7 t
