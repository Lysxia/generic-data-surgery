{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Operate on data types: insert\/modify\/delete fields and constructors.

module Generic.Data.Surgery.Internal where

import Control.Monad ((<=<))
import Data.Bifunctor (bimap, first)
import Data.Coerce
import Data.Functor.Identity (Identity)
import Data.Kind (Constraint, Type)
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits

import Fcf
  ( Eval, If, _If, IsBool, Pure, Pure2, Bimap, Uncurry
  , type (=<<), type (<=<), type (<$>)
  )

import Generic.Data.Internal.Compat (Div)
import Generic.Data.Internal.Data (Data(Data,unData))
import Generic.Data.Internal.Meta (MetaOf, MetaConsName, UnM1)
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
newtype OR (l :: k -> Type) (x :: k) = OR { unOR :: l x }

-- | /Move fresh data to the Operating Room, where surgeries can be applied./
--
-- Convert a generic type to a generic representation.
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

-- | The simplified generic representation type of type @a@,
-- that 'toOR' and 'fromOR' convert to and from.
type OROf a = OR (Linearize (Rep a)) ()

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

--

-- | @'removeCField' \@n \@t@: remove the @n@-th field of type @t@
-- in a non-record single-constructor type.
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
-- OR lt x      -- Data with field
-- ->
-- (t, OR l x)  -- Field value × Data without field
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
-- OR lt x      -- Data with field
-- ->
-- (t, OR l x)  -- Field value × Data without field
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
-- (t, OR l x)  -- Field value × Data without field
-- ->
-- OR lt x      -- Data with field
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
-- (t, OR l x)  -- Field value × Data without field
-- ->
-- OR lt x      -- Data with field
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
-- OR lt  x   -- Data with field t
-- ->
-- OR lt' x   -- Data with field t'
-- @
--
-- ==== Functional dependencies
--
-- @
-- n  lt    -> t  l
-- n  lt'   -> t' l
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
-- OR lt  x   -- Data with field t
-- ->
-- OR lt' x   -- Data with field t'
-- @
--
-- ==== Functional dependencies
--
-- @
-- fd lt    -> n  t  l
-- fd lt'   -> n  t' l
-- n  lt    -> fd t  l
-- n  lt'   -> fd t' l
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
-- @()@ and 'Data.Functor.Identity.Identity' can be used as an empty and a
-- singleton tuple.
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
-- OR lc x            -- Data with constructor
-- ->
-- Either t (OR l x)  -- Constructor (as a tuple) | Data without constructor
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
  :: forall    c t n lc l l_t x
  .  RmvConstr c t n lc l l_t
  => OR lc x -> Either t (OR l x)
removeConstr (OR a) = bimap
  (to . coerce' . gArborify @(Arborify l_t)) OR (gRemoveConstr @n a)

-- | A variant of 'removeConstr' that can infer the tuple type @t@ to hold
-- the contents of the removed constructor.
--
-- @t@ must be one of @()@, @Identity@, @(,)@ and actual tuples up to size 7
-- (because that's where 'Generic' instances currently stop).
--
-- === __Details__
--
-- See 'removeConstr'.
--
-- ==== Extra functional dependency
--
-- @
-- l_t -> t
-- @
removeConstrT
  :: forall    c t n lc l l_t x
  .  RmvConstrT c t n lc l l_t
  => OR lc x -> Either t (OR l x)
removeConstrT = removeConstr @c @t @n

-- | @'insertConstr' \@\"C\" \@n \@t@: insert a constructor @C@ at position @n@
-- with contents isomorphic to the tuple @t@.
--
-- @()@ and 'Data.Functor.Identity.Identity' can be used as an empty and a
-- singleton tuple.
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
-- Either t (OR l x)  -- Constructor (as a tuple) | Data without constructor
-- ->
-- OR lc x            -- Data with constructor
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
  :: forall    c t n lc l l_t x
  .  InsConstr c t n lc l l_t
  => Either t (OR l x) -> OR lc x
insertConstr z =
  OR (gInsertConstr @n
    (bimap (gLinearize @(Arborify l_t) . coerce' . from) unOR z))

-- | A variant of 'insertConstr' that can infer the tuple type @t@ to hold
-- the contents of the inserted constructor.
--
-- @t@ must be one of @()@, @Identity@, @(,)@ and actual tuples up to size 7
-- (because that's where 'Generic' instances currently stop).
--
-- === __Details__
--
-- See 'insertConstr'.
--
-- ==== Extra functional dependency
--
-- @
-- l_t -> t
-- @
insertConstrT
  :: forall    c t n lc l l_t x
  .  InsConstrT c t n lc l l_t
  => Either t (OR l x) -> OR lc x
insertConstrT = insertConstr @c @t @n

-- | @'modifyConstr' \@\"C\" \@n \@t \@t'@: modify the @n@-th constructor,
-- named @C@, with contents isomorphic to the tuple @t@, to another tuple @t'@.
--
-- @()@ and 'Data.Functor.Identity.Identity' can be used as an empty and a
-- singleton tuple.
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
-- (t -> t')
-- ->
-- OR lc  x  -- Data with initial constructor
-- ->
-- OR lc' x  -- Data with final   constructor
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
  :: forall    c t t' n lc lc' l l_t l_t' x
  .  ModConstr c t t' n lc lc' l l_t l_t'
  => (t -> t') -> OR lc x -> OR lc' x
modifyConstr f = insertConstr @c @t' @n . first f . removeConstr @c @t @n

-- | A variant of 'modifyConstr' that can infer the tuple types @t@ and @t'@ to
-- hold the contents of the inserted constructor.
--
-- @t@ and @t'@ must be one of @()@, @Identity@, @(,)@ and actual tuples up to
-- size 7 (because that's where 'Generic' instances currently stop).
--
-- === __Details__
--
-- See 'modifyConstr'.
--
-- ==== Extra functional dependency
--
-- @
-- l_t -> t
-- @
modifyConstrT
  :: forall     c t t' n lc lc' l l_t l_t' x
  .  ModConstrT c t t' n lc lc' l l_t l_t'
  => (t -> t') -> OR lc x -> OR lc' x
modifyConstrT = modifyConstr @c @t @t' @n

--

-- | This constraint means that the (unnamed) field row @lt@ contains
-- a field of type @t@ at position @n@, and removing it yields row @l@.
type RmvCField n t lt l =
  ( GRemoveField n lt
  , CFieldSurgery n t lt l
  )

-- | This constraint means that the record field row @lt@ contains a field of
-- type @t@ named @fd@ at position @n@, and removing it yields row @l@.
type RmvRField fd n t lt l =
  ( GRemoveField n lt
  , RFieldSurgery fd n t lt l
  )

-- | This constraint means that inserting a field @t@ at position @n@ in the
-- (unnamed) field row @l@ yields row @lt@.
type InsCField n t lt l =
  ( GInsertField n lt
  , CFieldSurgery n t lt l
  )

-- | This constraint means that inserting a field @t@ named @fd@ at position
-- @n@ in the record field row @l@ yields row @lt@.
type InsRField fd n t lt l =
  ( GInsertField n lt
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
type RmvConstr c t n lc l l_t =
  ( GRemoveConstr n lc
  , GArborify (Arborify l_t)
  , ConstrSurgery c t n lc l l_t
  )

-- | A variant of 'RmvConstr' allowing @t@ to be inferred.
type RmvConstrT c t n lc l l_t =
  ( RmvConstr c t n lc l l_t
  , IsTuple (Arity l_t) t
  )

-- | This constraint means that inserting a constructor @c@ at position @n@
-- in the constructor row @l@ yields row @lc@.
-- Furthermore, constructor @c@ contains a field row @l_t@ compatible with the
-- tuple type @t@.
type InsConstr c t n lc l l_t =
  ( GInsertConstr n lc
  , GLinearize (Arborify l_t)
  , ConstrSurgery c t n lc l l_t
  )

-- | A variant of 'InsConstr' allowing @t@ to be inferred.
type InsConstrT c t n lc l l_t =
  ( InsConstr c t n lc l l_t
  , IsTuple (Arity l_t) t
  )

-- | This constraint means that the constructor row @lc@ contains a constructor
-- named @c@ at position @n@ of type isomorphic to @t@, and modifying it to
-- @t'@ yields row @lc'@.
type ModConstr c t t' n lc lc' l l_t l_t' =
  ( RmvConstr c t  n lc  l l_t
  , InsConstr c t' n lc' l l_t'
  )

-- | A variant of 'ModConstr' allowing @t@ and @t'@ to be inferred.
type ModConstrT c t t' n lc lc' l l_t l_t' =
  ( ModConstr c t t' n lc lc' l l_t l_t'
  , IsTuple (Arity l_t) t
  , IsTuple (Arity l_t') t'
  )

type FieldSurgery n t lt l =
  ( t ~ Eval (FieldTypeAt n lt)
  , l ~ Eval (RemoveField n lt)
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

type ConstrSurgery c t n lc l l_t =
  ( Generic t
  , MatchFields (UnM1 (Rep t)) (Arborify l_t)
  , Coercible (Arborify l_t) (Rep t)
  , n ~ Eval (ConstrIndex c lc)
  , c ~ MetaConsName (MetaOf l_t)
  , l_t ~ Linearize (Arborify l_t)
  , l_t ~ Eval (ConstrAt n lc)
  , lc ~ Eval (InsertConstr n l_t l)
  , l ~ Eval (RemoveConstr n lc)
  )

--

type family   Linearize (f :: k -> *) :: k -> *
type instance Linearize (M1 D m f) = M1 D m (LinearizeSum f V1)
type instance Linearize (M1 C m f) = M1 C m (LinearizeProduct f U1)

type family   LinearizeSum (f :: k -> *) (tl :: k -> *) :: k -> *
type instance LinearizeSum V1 tl = tl
type instance LinearizeSum (f :+: g) tl = LinearizeSum f (LinearizeSum g tl)
type instance LinearizeSum (M1 c m f) tl = M1 c m (LinearizeProduct f U1) :+: tl

type family   LinearizeProduct (f :: k -> *) (tl :: k -> *) :: k -> *
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

type family   Arborify (f :: k -> *) :: k -> *
type instance Arborify (M1 D m f) = M1 D m (Eval (ArborifySum (CoArity f) f))
type instance Arborify (M1 C m f) = M1 C m (Eval (ArborifyProduct (Arity f) f))

data ArborifySum (n :: Nat) (f :: k -> *) :: (k -> *) -> *
type instance Eval (ArborifySum n V1) = V1
type instance Eval (ArborifySum n (f :+: g)) =
  Eval (If (n == 1)
    (ArborifyProduct (Arity f) f)
    (Arborify' ArborifySum (:+:) n (Div n 2) f g))

data ArborifyProduct (n :: Nat) (f :: k -> *) :: (k -> *) -> *
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

data SplitAt :: Nat -> (k -> *) -> (k -> *, k -> *) -> *
type instance Eval (SplitAt n (f :+: g)) =
  Eval (If (n == 0)
    (Pure '(V1, f :+: g))
    (Bimap (Pure2 (:+:) f) Pure =<< SplitAt (n-1) g))
type instance Eval (SplitAt n (f :*: g)) =
  Eval (If (n == 0)
    (Pure '(U1, f :*: g))
    (Bimap (Pure2 (:*:) f) Pure =<< SplitAt (n-1) g))

data FieldTypeAt (n :: Nat) (f :: k -> *) :: * -> *
type instance Eval (FieldTypeAt n (M1 i c f)) = Eval (FieldTypeAt n f)
type instance Eval (FieldTypeAt n (f :+: V1)) = Eval (FieldTypeAt n f)
type instance Eval (FieldTypeAt n (f :*: g)) =
  Eval (If (n == 0) (Pure (FieldTypeOf f)) (FieldTypeAt (n-1) g))

type family   FieldTypeOf (f :: k -> *) :: *
type instance FieldTypeOf (M1 s m (K1 i a)) = a

data RemoveField (n :: Nat) (f :: k -> *) :: (k -> *) -> *
type instance Eval (RemoveField n (M1 i m f)) = M1 i m (Eval (RemoveField n f))
type instance Eval (RemoveField n (f :+: V1)) = Eval (RemoveField n f) :+: V1
type instance Eval (RemoveField n (f :*: g)) =
  Eval (If (n == 0) (Pure g) ((:*:) f <$> RemoveField (n-1) g))

type DefaultMetaSel field
  = 'MetaSel field 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy

data InsertField (n :: Nat) (fd :: Maybe Symbol) (t :: *) (f :: k -> *) :: (k -> *) -> *
type instance Eval (InsertField n fd t (M1 D m f)) = M1 D m (Eval (InsertField n fd t f))
type instance Eval (InsertField n fd t (M1 C m f)) = M1 C m (Eval (InsertField n fd t f))
type instance Eval (InsertField n fd t (f :+: V1)) = Eval (InsertField n fd t f) :+: V1
type instance Eval (InsertField n fd t (f :*: g)) =
  Eval (If (n == 0)
    (Pure (M1 S (DefaultMetaSel fd) (K1 R t) :*: (f :*: g)))
    ((:*:) f <$> InsertField (n-1) fd t g))
type instance Eval (InsertField 0 fd t U1) = M1 S (DefaultMetaSel fd) (K1 R t) :*: U1

data Succ :: Nat -> Nat -> *
type instance Eval (Succ n) = 1 + n

-- | Position of a record field
data FieldIndex (field :: Symbol) (f :: k -> *) :: Nat -> *
type instance Eval (FieldIndex field (M1 D m f)) = Eval (FieldIndex field f)
type instance Eval (FieldIndex field (M1 C m f)) = Eval (FieldIndex field f)
type instance Eval (FieldIndex field (f :+: V1)) = Eval (FieldIndex field f)
type instance Eval (FieldIndex field (M1 S ('MetaSel ('Just field') su ss ds) f :*: g))
  = Eval (If (field == field') (Pure 0) (Succ =<< FieldIndex field g))

-- | Number of fields of a single constructor
type family   Arity (f :: k -> *) :: Nat
type instance Arity (M1 d m f) = Arity f
type instance Arity (f :+: V1) = Arity f
type instance Arity (f :*: g) = Arity f + Arity g
type instance Arity (K1 i c) = 1
type instance Arity U1 = 0

-- | Number of constructors of a data type
type family   CoArity (f :: k -> *) :: Nat
type instance CoArity (M1 D m f) = CoArity f
type instance CoArity (M1 C m f) = 1
type instance CoArity V1         = 0
type instance CoArity (f :+: g)  = CoArity f + CoArity g

class GRemoveField (n :: Nat) f where
  gRemoveField :: f x -> (Eval (FieldTypeAt n f), Eval (RemoveField n f) x)

instance GRemoveField n f => GRemoveField n (M1 i c f) where
  gRemoveField (M1 a) = M1 <$> gRemoveField @n a

instance GRemoveField n f => GRemoveField n (f :+: V1) where
  gRemoveField (L1 a) = L1 <$> gRemoveField @n a
  gRemoveField (R1 v) = absurd1 v

instance (If (n == 0) (() :: Constraint) (GRemoveField (n-1) g), IsBool (n == 0))
  => GRemoveField n (M1 s m (K1 i t) :*: g) where
  gRemoveField (a@(M1 (K1 t)) :*: b) = _If @(n == 0)
    (t, b)
    ((a :*:) <$> gRemoveField @(n-1) b)

class GInsertField (n :: Nat) f where
  gInsertField :: Eval (FieldTypeAt n f) -> Eval (RemoveField n f) x -> f x

instance GInsertField n f => GInsertField n (M1 i c f) where
  gInsertField t (M1 a) = M1 (gInsertField @n t a)

instance GInsertField n f => GInsertField n (f :+: V1) where
  gInsertField t (L1 a) = L1 (gInsertField @n t a)
  gInsertField _ (R1 v) = absurd1 v

instance (If (n == 0) (() :: Constraint) (GInsertField (n-1) g), IsBool (n == 0))
  => GInsertField n (M1 s m (K1 i t) :*: g) where
  gInsertField t ab = _If @(n == 0)
    (M1 (K1 t) :*: ab)
    (let a :*: b = ab in a :*: gInsertField @(n-1) t b)

data ConstrAt (n :: Nat) (f :: k -> *) :: (k -> *) -> *
type instance Eval (ConstrAt n (M1 i m f)) = Eval (ConstrAt n f)
type instance Eval (ConstrAt n (f :+: g)) =
  Eval (If (n == 0) (Pure f) (ConstrAt (n-1) g))

data RemoveConstr (n :: Nat) (f :: k -> *) :: (k -> *) -> *
type instance Eval (RemoveConstr n (M1 i m f)) = M1 i m (Eval (RemoveConstr n f))
type instance Eval (RemoveConstr n (f :+: g)) =
  Eval (If (n == 0) (Pure g) ((:+:) f <$> RemoveConstr (n-1) g))

data InsertConstr (n :: Nat) (t :: k -> *) (f :: k -> *) :: (k -> *) -> *
type instance Eval (InsertConstr n t (M1 i m f)) = M1 i m (Eval (InsertConstr n t f))
type instance Eval (InsertConstr n t (f :+: g)) =
  Eval (If (n == 0) (Pure (t :+: (f :+: g))) ((:+:) f <$> InsertConstr (n-1) t g))
type instance Eval (InsertConstr 0 t V1) = t :+: V1

data ConstrIndex (con :: Symbol) (f :: k -> *) :: Nat -> *
type instance Eval (ConstrIndex con (M1 D m f)) = Eval (ConstrIndex con f)
type instance Eval (ConstrIndex con (M1 C ('MetaCons con' fx s) f :+: g)) =
  Eval (If (con == con') (Pure 0) (Succ =<< ConstrIndex con g))

class GRemoveConstr (n :: Nat) f where
  gRemoveConstr :: f x -> Either (Eval (ConstrAt n f) x) (Eval (RemoveConstr n f) x)

instance GRemoveConstr n f => GRemoveConstr n (M1 i c f) where
  gRemoveConstr (M1 a) = M1 <$> gRemoveConstr @n a

instance (If (n == 0) (() :: Constraint) (GRemoveConstr (n-1) g), IsBool (n == 0))
  => GRemoveConstr n (f :+: g) where
  gRemoveConstr = _If @(n == 0)
    (\case
      L1 a -> Left a
      R1 b -> Right b)
    (\case
      L1 a -> Right (L1 a)
      R1 b -> R1 <$> gRemoveConstr @(n-1) b)

class GInsertConstr (n :: Nat) f where
  gInsertConstr :: Either (Eval (ConstrAt n f) x) (Eval (RemoveConstr n f) x) -> f x

instance GInsertConstr n f => GInsertConstr n (M1 i c f) where
  gInsertConstr = M1 . gInsertConstr @n . fmap unM1

instance (If (n == 0) (() :: Constraint) (GInsertConstr (n-1) g), IsBool (n == 0))
  => GInsertConstr n (f :+: g) where
  gInsertConstr = _If @(n == 0)
    (\case
      Left a -> L1 a
      Right b -> R1 b)
    (\case
      Left a -> R1 (gInsertConstr @(n-1) (Left a))
      Right (L1 a) -> L1 a
      Right (R1 b) -> R1 (gInsertConstr @(n-1) (Right b)))

-- | Generate equality constraints between fields of two matching generic
-- representations.
class MatchFields (f :: k -> *) (g :: k -> *)
instance (g' ~ M1 D d g, MatchFields f g) => MatchFields (M1 D c f) g'
-- Forcing the MetaCons field
instance (g' ~ M1 C ('MetaCons _cn _s _t) g, MatchFields f g)
  => MatchFields (M1 C c f) g'
instance (g' ~ M1 S d g, MatchFields f g) => MatchFields (M1 S c f) g'
instance (g' ~ (g1 :+: g2), MatchFields f1 g1, MatchFields f2 g2)
  => MatchFields (f1 :+: f2) g'
instance (g' ~ (g1 :*: g2), MatchFields f1 g1, MatchFields f2 g2)
  => MatchFields (f1 :*: f2) g'
instance (g' ~ K1 j a) => MatchFields (K1 i a) g'
instance (g' ~ U1) => MatchFields U1 g'
instance (g' ~ V1) => MatchFields V1 g'

class IsTuple (n :: Nat) (t :: k)
instance (t ~ ())                    => IsTuple 0 t
instance (t ~ Identity a)            => IsTuple 1 t
instance (t ~ (a, b))                => IsTuple 2 t
instance (t ~ (a, b, c))             => IsTuple 3 t
instance (t ~ (a, b, c, d))          => IsTuple 4 t
instance (t ~ (a, b, c, d, e))       => IsTuple 5 t
instance (t ~ (a, b, c, d, e, f))    => IsTuple 6 t
instance (t ~ (a, b, c, d, e, f, g)) => IsTuple 7 t

-- | Unify the "spines" of two generic representations (the "spine" is
-- everything except the field types).
class UnifyRep (f :: k -> *) (g :: k -> *)
instance (g' ~ M1 D c g, UnifyRep f g) => UnifyRep (M1 D c f) g'
instance (g' ~ M1 C c g, UnifyRep f g)
  => UnifyRep (M1 C c f) g'
instance (g' ~ M1 S c g, UnifyRep f g) => UnifyRep (M1 S c f) g'
instance (g' ~ (g1 :+: g2), UnifyRep f1 g1, UnifyRep f2 g2)
  => UnifyRep (f1 :+: f2) g'
instance (g' ~ (g1 :*: g2), UnifyRep f1 g1, UnifyRep f2 g2)
  => UnifyRep (f1 :*: f2) g'
instance (g' ~ K1 i b) => UnifyRep (K1 i a) g'
instance (g' ~ U1) => UnifyRep U1 g'
instance (g' ~ V1) => UnifyRep V1 g'

-- | Can be used with @generic-lens@ for type-changing field updates.
--
-- A specialization of the identity function to be used to fix types
-- of functions using 'Data' as input or output, unifying the "spines" of input
-- and output generic representations (the "spine" is everything except field
-- types, which may thus change).
onData
  :: (UnifyRep (Rep a) (Rep b), UnifyRep (Rep a) (Rep b))
  => p a b -> p a b
onData = id
