-- | Surgery for generic data types:
-- remove and insert constructors and fields.
--
-- Functions in this module are expected to be used with visible type
-- applications. Surgeries have a lot of type parameters, but usually only the
-- first one to three type arguments need to be passed via @TypeApplications@.
-- Functions are documented with informal \"functional dependencies\",
-- clarifying which type parameters can be inferred from which others
-- (click on \"Details\" under each function to see those).
--
-- Remember that not all parameters to the left of a functional dependency
-- arrow need to be annotated explicitly to determine those on the right. Some
-- can also be inferred from the context.
--
-- Note that constructors and fields are indexed from zero.

module Generic.Data.Surgery
  ( -- * Surgeries from generic-data and generic-lens
    --
    -- | The library <https://hackage.haskell.org/package/generic-data generic-data>
    -- has a "Generic.Data.Microsurgery" module (since 0.4.0.0) to modify some
    -- metadata of generic representations.
    --
    -- If you only want to /update/ fields, rather than remove or insert them,
    -- see also the documentation in the above module, on making surgeries out of
    -- <https://hackage.haskell.org/package/generic-data generic-lens>.

    -- * Synthetic data types

    Data

  , toData
  , fromData

    -- * Surgeries

    -- ** Getting into the operating room
  , OR

  , toOR
  , fromOR'
  , toOR'
  , fromOR

  , OROf

  , toORLazy
  , fromORLazy

  , OROfLazy

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
  , ToORRepLazy
  , ToORLazy
  , FromORRep
  , FromOR
  , FromORRepLazy
  , FromORLazy

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

import Generic.Data.Types (Data(..), toData, fromData)

import Generic.Data.Surgery.Internal
