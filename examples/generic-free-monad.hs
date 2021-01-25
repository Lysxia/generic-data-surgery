{- Derive Monad for a type like @Pipe@, which is isomorphic to a @Free@ monad
 - using generic-data-surgery and generic-functor
 -}

{-# LANGUAGE
  AllowAmbiguousTypes,
  ConstraintKinds,
  DataKinds,
  DeriveGeneric,
  DerivingVia,
  FlexibleInstances,
  GADTs,
  KindSignatures,
  MultiParamTypeClasses,
  PolyKinds,
  QuantifiedConstraints,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneDeriving,
  TypeApplications,
  TypeFamilies,
  TypeOperators,
  UndecidableInstances
  #-}
import Generic.Functor
import Generic.Data.Surgery (Data)
import qualified Generic.Data.Surgery as GDS
import GHC.Generics (Generic(Rep), K1(..), M1(..))

import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Functor.Identity (Identity(..))
import GHC.TypeLits (Nat, Symbol)
import Data.Type.Bool
import Data.Type.Equality (type (==))

import Data.Bifunctor (first)
import Control.Monad (ap)

data Pipe i o a
  = Input (i -> Pipe i o a)
  | Output o (Pipe i o a)
  | Return a
  deriving Generic

deriving via GenericFunctor (Pipe i o) instance Functor (Pipe i o)
deriving via GenericFreeMonad "Return" (Pipe i o) instance Applicative (Pipe i o)
deriving via GenericFreeMonad "Return" (Pipe i o) instance Monad (Pipe i o)

--

example :: Pipe Int Bool ()
example = do
  x <- Input Return
  Output (x > 10) (Return ())
  if x == 0 then Return ()
  else example

runPipe :: [i] -> Pipe i o r -> ([o], r)
runPipe (i : is) (Input k) = runPipe is (k i)
runPipe is (Output o k) = first (o :) (runPipe is k)
runPipe is (Return r) = ([], r)
runPipe [] (Input _) = error "Pipe burst" 

main :: IO ()
main = do
  let r = runPipe [1,11,2,22,0] example
  if r == ([False,True,False,True,False],()) then
    pure ()
  else
    error ("Unexpected result :" ++ show r)

-- Generic library

{- = Recipe

   Given a type like @Pipe@, which is @Generic@, and a constructor name such as @Return@
   provided as a @Symbol@, we implement @pure@ and @(>>=)@ generically.
   @pure@ is easy enough compared to @(>>=)@.

   To implement @(>>=)@, we define @(>>= k) :: m a -> m b@ recursively by case analysis
   on the first argument:

   1. If the first argument is the constructor @Return@ with a single field @a@, then apply @k@
   2. For all other constructors, recursively apply @(>>= k)@ wherever it makes
      sense.

   We use the library generic-data-surgery (@InsertConstr@, @RemoveConstr@) to extract the
   @Return@ constructor (step 1), and generic-functor (@gsolomap@) to apply @(>>= k)@ everywhere
   else.
 -}

--

newtype GenericFreeMonad (returnName :: Symbol) (m :: Type -> Type) (a :: Type)
  = GenericFreeMonad (m a)

deriving via (m :: Type -> Type) instance Functor m => Functor (GenericFreeMonad returnName m)

instance GMonad returnName m => Applicative (GenericFreeMonad returnName m) where
  (<*>) = ap
  pure = coercePure (gpure @returnName @m) where

instance GMonad returnName m => Monad (GenericFreeMonad returnName m) where
  (>>=) = coerceBind (gbind @returnName @m) where

coerceBind :: Coercible m n => (m a -> (a -> m b) -> m b) -> (n a -> (a -> n b) -> n b)
coerceBind = coerce

coercePure :: Coercible m n => (a -> m a) -> (a -> n a)
coercePure = coerce

--

class (Functor m, forall a. GPure returnName m a, forall a b. GBind returnName m a b)
  => GMonad (returnName :: Symbol) (m :: Type -> Type)
instance (Functor m, forall a. GPure returnName m a, forall a b. GBind returnName m a b)
  => GMonad (returnName :: Symbol) (m :: Type -> Type)

class GBind (returnName :: Symbol) (m :: Type -> Type) a b where
  gbind :: m a -> (a -> m b) -> m b

-- 1. Separate the @Return@ constructor using the @RemoveConstr@ surgery
-- 2. Apply @k@ to the single field of @Return@ (@LookupS "k"@)
-- 3. Apply @(>>= k)@ to every field of every other constructor (@GSolomapS@)
-- 3b. Clean up: restore the generic representation (@InsertConstr@) and
--     convert back to the original type @m@.
--
-- We have to manually generalize @l, lc, f, f', l', lc'@, otherwise GHC will specialize them
-- to @Any@, which is bad.
type GBindSurgery (returnName :: Symbol) (n :: Nat) (m :: Type -> Type) (a :: Type) (b :: Type)
  l lc f f' l' lc' =
  ((ToOR
    :>>> RemoveConstr @lc @() @l returnName n (Identity a)
    :>>> CaseEither
      (RunIdentity :>>> LookupS @a @(m b) "k")
      (FromOR' @l @() @f :>>> GSolomapS (LookupS @(m a) @(m b) "go_k") :>>> ToOR' @f' @() @l'
        :>>> RightS :>>> InsertConstr @l' @() @lc' returnName n (Identity b) :>>> FromOR))
    :: m a ~~> m b)

instance (s ~ GBindSurgery returnName n m a b l lc f f' l' lc', Apply s)
  => GBind returnName m a b where
  gbind u k = go_k u where
    go_k :: m a -> m b
    go_k = apply @(m a) @(m b) @s (Tagged @"k" @(a -> m b) k :+ Tagged @"go_k" @(m a -> m b) go_k :+ ())

class GPure (returnName :: Symbol) (m :: Type -> Type) a where
  gpure :: a -> m a

type GPureSurgery (returnName :: Symbol) (n :: Nat) (m :: Type -> Type) (a :: Type)
  l lc =
  (IdentityS :>>> LeftS :>>> InsertConstr @l @() @lc returnName n (Identity a) :>>> FromOR
    :: a ~~> m a)

instance (s ~ GPureSurgery returnName n m a l lc, Apply s)
  => GPure returnName m a where
  gpure = apply @a @(m a) @s ()

--

-- generic-data-surgery is currently a pain to use parametrically.
-- We build a type-level language embedding generic-data-surgery
-- in order to "postpone" some constraint solving to use sites of
-- @gbind@/@gpure@/DerivingVia, where the monad is concretely known.

type a ~~> b = (a -> b -> Type)

class Apply (f :: a ~~> b) where
  type Env (f :: a ~~> b) (e :: Type) :: Constraint
  type Env f e = (() :: Constraint)
  apply :: Env f e => e -> a -> b

data Id :: a ~~> a

instance Apply Id where
  apply _ = id

data (f :: a ~~> b) :>>> (g :: b ~~> c) :: a ~~> c

instance (Apply f, Apply g) => Apply (f :>>> g) where
  type Env (f :>>> g) e = (Env f e, Env g e)
  apply e = apply @_ @_ @g e . apply @_ @_ @f e

data CaseEither (f :: a ~~> c) (g :: b ~~> c) :: Either a b ~~> c

instance (Apply f, Apply g) => Apply (CaseEither f g) where
  type Env (CaseEither f g) e = (Env f e, Env g e)
  apply e = either (apply @_ @_ @f e) (apply @_ @_ @g e)

data ToOR_ (k :: GDS.OR l x ~~> d) :: a ~~> d
type ToOR = ToOR_ Id

instance (Apply k, Generic a, GDS.ToORRep a l)
  => Apply (ToOR_ (k :: GDS.OR l x ~~> d) :: a ~~> d) where
  type Env (ToOR_ k) e = Env k e
  apply e = apply @_ @_ @k e . GDS.toOR

data FromOR_ (k :: d ~~> GDS.OR l x) :: d ~~> a
type FromOR = FromOR_ Id

instance (Apply k, Generic a, GDS.FromORRep a l)
  => Apply (FromOR_ (k :: d ~~> GDS.OR l x) :: d ~~> a) where
  type Env (FromOR_ k) e = Env k e
  apply e = GDS.fromOR . apply @_ @_ @k e

data ToOR' :: Data f x ~~> GDS.OR l x

instance (GDS.ToOR f l)
  => Apply (ToOR' :: Data f x ~~>  GDS.OR l x) where
  apply _ = GDS.toOR'

data FromOR' :: GDS.OR l x ~~> Data f x

instance (GDS.FromOR f l)
  => Apply (FromOR' :: GDS.OR l x ~~> Data f x) where
  apply _ = GDS.fromOR'

data InsertConstr (c :: Symbol) (n :: Nat) (t :: Type) :: Either t (GDS.OR l (x :: Type)) ~~> GDS.OR lc x

instance (GDS.InsConstr c n t lc l)
  => Apply (InsertConstr c n t :: Either t (GDS.OR l x) ~~> GDS.OR lc x) where
  apply _ = GDS.insertConstr @c @n @t @lc @l

data RemoveConstr (c :: Symbol) (n :: Nat) (t :: Type) :: GDS.OR lc x ~~> Either t (GDS.OR l x)

instance (GDS.RmvConstr c n t lc l)
  => Apply (RemoveConstr c n t :: GDS.OR lc x ~~> Either t (GDS.OR l x)) where
  apply _ = GDS.removeConstr @c @n @t @lc @l

newtype Tagged s a = Tagged a

class Lookup s e where
  type Assoc s e :: Type
  lookupEnv :: e -> Assoc s e

data SBool (b :: Bool) where
  SFalse :: SBool 'False
  STrue :: SBool 'True

class IsBool b where
  demoteBool :: SBool b

instance IsBool 'False where
  demoteBool = SFalse

instance IsBool 'True where
  demoteBool = STrue

instance (IsBool (s == s'), If (s == s') (() :: Constraint) (Lookup s b)) => Lookup s (Tagged s' a :+ b) where
  type Assoc s (Tagged s' a :+ b) = If (s == s') a (Assoc s b)
  lookupEnv (Tagged a :+ b) = case demoteBool @(s == s') of
    STrue -> a
    SFalse -> lookupEnv @s b

data GSolomapS (k :: a ~~> b) :: x ~~> y

instance (Apply k, Generic x, Generic y, GSolomap a b x y) => Apply (GSolomapS (k :: a ~~> b) :: x ~~> y) where
  type Env (GSolomapS k) e = Env k e
  apply e = gsolomap (apply @_ @_ @k e)

data LookupS (s :: Symbol) :: a ~~> b

instance Apply (LookupS s :: a ~~> b) where
  type Env (LookupS s) e = (Lookup s e, Assoc s e ~ (a -> b))
  apply e = lookupEnv @s e

data RunIdentity :: Identity a ~~> a

instance Apply RunIdentity where
  apply _ = runIdentity

data IdentityS :: a ~~> Identity a

instance Apply IdentityS where
  apply _ = Identity

data LeftS :: e ~~> Either e a

instance Apply LeftS where
  apply _ = Left

data RightS :: a ~~> Either e a

instance Apply RightS where
  apply _ = Right
