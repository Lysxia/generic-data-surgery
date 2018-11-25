-- | Surgery for generic data types:
-- remove and insert constructors and fields.
--
-- Functions in this module are expected to be used with visible type
-- applications. Surgeries have a lot of type parameters, but usually only the
-- first one to three type arguments need to be passed via @TypeApplications@.
-- Functions are annotated with \"functional dependencies\", with a meaning
-- similar to the homonymous GHC extension for type classes (click on
-- \"Details\" under each function to see those).
--
-- Remember that not all parameters to the left of a functional dependency
-- arrow need to be annotated explicitly to determine those on the right. Some
-- can also be inferred from the context.
--
-- Note that constructors and fields are indexed from zero.

module Generic.Data.Surgery
  ( Data

  , toData
  , fromData
  , onData

    --   Microsurgery

    --   One common and simple situation is to wrap a couple of fields in some
    -- newtype. You can leverage the @generic-lens@ library with the three
    -- functions below.
    --
    -- @
    -- over :: ASetter s t a b -> (a -> b) -> s -> t  -- from lens or microlens
    -- field :: HasField s t a b => Lens s t a b      -- from generic-lens
    -- @
    --
    -- For example, to wrap a field named @hidden@ in a newtype like
    -- 'Generic.Data.Opaque' in some record type @R@:
    --
    -- @
    -- 'onData' (over (field @"hidden") 'Generic.Data.Opaque') . 'toData'
    --   :: R -> Data _ _
    -- @
    --
    -- The result is a type, that from the point of view of "GHC.Generics"
    -- looks just like @R@ but with the field @hidden@ wrapped.

    -- * Getting into the operating room
  , OR

  , toOR
  , fromOR'
  , toOR'
  , fromOR

  , OROf

    -- * Surgeries

    -- ** Unnamed fields
  , removeCField
  , insertCField
  , insertCField'
  , modifyCField

    -- ** Named fields (records)
  , removeRField
  , insertRField
  , insertRField'
  , modifyRField

    -- ** Constructors

    -- | A constructor is extracted to a "tuple", which can be any
    -- 'GHC.Generics.Generic' single-constructor type with the same number of
    -- fields.
    --
    -- Note that @()@ and 'Data.Functor.Identity.Identity' can be used as an
    -- empty and a singleton tuple type respectively.
    --
    -- When the tuple type can't be inferred and doesn't really matter,
    -- an alternative to explicit type annotations is to use the @...ConstrT@
    -- variants of these surgeries, which are specialized to actual tuples
    -- (@()@, 'Data.Functor.Identity.Identity', @(,)@, @(,,)@, up to 7 ---
    -- because that's where 'GHC.Generics.Generic' instances currently stop).

  , removeConstr
  , insertConstr
  , modifyConstr
  , removeConstrT
  , insertConstrT
  , modifyConstrT

    -- * Constraint synonyms

    -- | Hiding implementation details from the signatures above.
    -- Useful to compose surgeries in a reusable way.

    -- ** Conversions

  , ToORRep
  , ToOR
  , FromORRep
  , FromOR

    -- ** Surgeries

  , RmvCField
  , InsCField
  , ModCField
  , RmvRField
  , InsRField
  , ModRField
  , RmvConstr
  , InsConstr
  , ModConstr
  , RmvConstrT
  , InsConstrT
  , ModConstrT
  ) where

import Generic.Data.Internal.Data

import Generic.Data.Surgery.Internal
