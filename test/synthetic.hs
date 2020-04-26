{-# LANGUAGE
    CPP,
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    GeneralizedNewtypeDeriving,
    KindSignatures,
    PolyKinds,
    ScopedTypeVariables,
    StandaloneDeriving,
    TypeApplications,
    TypeFamilies,
    TypeInType,
    TypeOperators,
    UndecidableInstances
  #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE DerivingStrategies #-}
#endif

#if 806 > __GLASGOW_HASKELL__
import Data.Coerce (Coercible, coerce)
#endif
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Identity (Identity(..))
import Data.Bifunctor (first, second, bimap)
import GHC.Generics (Generic(..))
import qualified GHC.Generics  -- Make constructors visible to Coercible
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data (GShow1)
import Generic.Data.Surgery

data RowId = RowId
  deriving Show

type InsertId = (InsertField 0 ('Just "pk") RowId :: MajorSurgery k)

newtype WithId a =
  WithId (Data (Operate (Rep a) InsertId) ())

deriving instance GShow1 (Operate (Rep a) InsertId) => Show (WithId a)

#if __GLASGOW_HASKELL__ >= 806
deriving newtype instance
  ( Generic a
  , Functor (Operate (Rep a) InsertId)
  , Contravariant (Operate (Rep a) InsertId)
  ) => Generic (WithId a)
#else
-- Without DerivingStrategies, we do newtype deriving of Generic by hand.
instance
  ( Generic a
  , Functor (Operate (Rep a) InsertId)
  , Contravariant (Operate (Rep a) InsertId)
  ) => Generic (WithId a) where
  type Rep (WithId a) = Operate (Rep a) InsertId
  to = to'
  from = from'

to' :: forall a x.
  (Coercible a (Data (Rep a) ()), Functor (Rep a), Contravariant (Rep a)) =>
  Rep a x -> a
to' = coerce (to @(Data (Rep a) ()) @x)

from' :: forall a x.
  (Coercible a (Data (Rep a) ()), Functor (Rep a), Contravariant (Rep a)) =>
  a -> Rep a x
from' = coerce (from @(Data (Rep a) ()) @x)
#endif

addKey ::
  ( Generic a
  , Perform (Rep a) InsertId
  ) => RowId -> a -> WithId a
addKey i = WithId . fromOR' . insertRField' @"pk" @0 @RowId i . toOR

data Woof = Waf { fluff :: Int }
  deriving Generic

type SemiFluff = RemoveRField "fluff" Int

type Fluffy = (SemiFluff :>> InsertField 0 ('Just "fluffy") Bool :: MajorSurgery k)

unfluff :: (Generic a, Perform (Rep a) SemiFluff)
  => a -> (Int, Data (Operate (Rep a) SemiFluff) ())
unfluff = fmap fromOR' . removeRField @"fluff" . toOR

fluffier :: (Generic a, Perform (Rep a) Fluffy) => a -> Data (Operate (Rep a) Fluffy) ()
fluffier = fromOR' . insertRField @"fluffy" @0 . first (>= 0) . removeRField @"fluff" . toOR

data Meow = Miaou Int
  deriving Generic

type UnMiaou = (RemoveConstr "Miaou" (Identity Int) :: MajorSurgery k)
type Paw = InsertConstrAt "Paw" 1 (Bool, Bool)

unMiaou :: (Generic a, Perform (Rep a) UnMiaou)
  => a -> Either Int (Data (Operate (Rep a) UnMiaou) ())
unMiaou = bimap runIdentity fromOR' . removeConstrT @"Miaou" . toOR

purr :: (Generic a, Perform (Rep a) Paw) =>
  Either (Bool, Bool) a -> Data (Operate (Rep a) Paw) ()
purr = fromOR' . insertConstrT @"Paw" @1 . second toOR

type Aww = (Paw :>> UnMiaou :: MajorSurgery k)

{-
pat :: forall a. (Generic a, Perform (Rep a) Aww) =>
  Either (Bool, Bool) a -> Either (Identity Int) (Data (Operate (Rep a) Aww) ())
pat = second fromOR' . removeConstrT @"Miaou" . insertConstrT @"Paw" @1 @(Bool, Bool) . second toOR
-}

main :: IO ()
main = defaultMain test

test :: TestTree
test = testGroup "synthetic"
  [ testCase "addKey" $ "WithId (Waf {pk = RowId, fluff = 77})" @?= show (addKey RowId (Waf 77))
  , testCase "unfluff" $ "(33,Waf {})" @?= show (unfluff (Waf 33))
  , testCase "fluffier" $ "Waf {fluffy = True}" @?= show (fluffier (Waf 33))
  , testCase "unMiaou" $ "Left 3" @?= show (unMiaou (Miaou 3))
  , testCase "purr" $ "Miaou 3" @?= show (purr (Right (Miaou 3)))
  ]
