{-# LANGUAGE
    DataKinds,
    DeriveFunctor,
    DeriveGeneric,
    DuplicateRecordFields,
    FlexibleInstances,
    OverloadedStrings,
    TypeApplications #-}

import Data.Aeson
import GHC.Generics (Generic)
import Generic.Data.Surgery (fromOR, toOR', modifyRField)

-- * The problem

-- @Rec@ is some record we want to deserialize from JSON.
-- Requirement: if the @"payload"@ field doesn't exist, make it the empty
-- string.

data Rec = Rec
  { iden    :: Int
  , header1 :: Int
  , header2 :: Int
  , payload :: String
  } deriving (Eq, Generic, Show)

-- ** Some unit tests

-- Example parameterized by the @Rec@ constructor.
example
  :: (Eq rec, Show rec, FromJSON rec)
  => (Int -> Int -> Int -> String -> rec) -> IO ()
example rec = do
  let ex1 = "{\"iden\":1,\"header1\":2,\"header2\":3}"
      ex2 = "{\"iden\":1,\"header1\":2,\"header2\":3,\"payload\":\"Nyalas\"}"
  assertEqual (Right (rec 1 2 3 ""))       (eitherDecode ex1)
  assertEqual (Right (rec 1 2 3 "Nyalas")) (eitherDecode ex2)

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual a b
  | a == b = pure ()
  | otherwise = do
    putStrLn $ "Expected: " ++ show a
    putStrLn $ "Actual:   " ++ show b
    fail "Assertion failed, not equal."

-- This helper will be convenient to have.
defString :: Maybe String -> String
defString (Just s) = s
defString Nothing = ""

-- ** First solution

-- Direct implementation, verbose but straightforward.
instance FromJSON Rec where
  parseJSON = withObject "Rec" $ \o -> do
    i  <- o .: "iden"
    h1 <- o .: "header1"
    h2 <- o .: "header2"
    p' <- o .:? "payload"
    return Rec{iden = i, header1 = h1, header2 = h2, payload = defString p'}

-- ** Second solution using plain GHC.Generics

-- One other basic solution, that uses aeson's generic deriving features, is to
-- parameterize @Rec@ so the @payload@ field can be decoded differently than
-- using the instance for @String@.
--
-- More precisely, we decode it as @Maybe String@, with the option
-- @omitNothingFields=True@.

data Rec2_ string = Rec2
  { iden    :: Int
  , header1 :: Int
  , header2 :: Int
  , payload :: string
  } deriving (Eq, Functor, Generic, Show)
-- We can use the @DeriveFunctor@ extension to make mapping over the @payload@
-- field painless.

type Rec2 = Rec2_ String

instance FromJSON Rec2 where
  parseJSON
    = (fmap . fmap) defString
    . genericParseJSON defaultOptions{omitNothingFields=True}

-- @genericParseJSON@ produces a @Parser (Rec2_ (Maybe String))@,
-- the result type then becomes @Rec2@ (i.e., @Rec2_ String@)
-- via @(fmap . fmap) defString@.

-- ** Third solution using GHC.Generics and also generic-data-surgery

-- This is exactly the same type as @Rec@, redeclared just so it can carry
-- another instance.
--
-- Instead of mangling our type like @Rec2@, generic-data-surgery creates a
-- new generic type with which to instantiate @genericParseJSON@, that is
-- isomorphic to @Rec2_ (Maybe String)@.

data Rec3 = Rec3
  { iden    :: Int
  , header1 :: Int
  , header2 :: Int
  , payload :: String
  } deriving (Eq, Generic, Show)

instance FromJSON Rec3 where
  parseJSON
    = fmap (fromOR . modifyRField @"payload" defString . toOR')
    . genericParseJSON defaultOptions{omitNothingFields=True}

-- @genericParseJSON@ produces a @Parser (Data ...)@, where @Data ...@ is a
-- generic type constructed by generic-data-surgery, and as far as
-- @GHC.Generics@ is concerned, that @Data ...@ is a record type with the same
-- fields as @Rec3@, except the @payload@ field has type @Maybe String@.
--
-- The @modifyRField@ function is a "surgery", which transforms the @payload@ field
-- from @Maybe String@ to @String@ using @defString@, so the record can finally be
-- converted to @Rec3@.
--
-- The surgery is confined to an "operating room" (@OR@), that interfaces with
-- regular Haskell data types on one side (via @fromOR@) and "synthetic generic
-- types" (@Data@) on the other side (via @toOR'@).
--
-- (@Data@ can be imported from @Generic.Data.Surgery@ but it actually comes
-- from generic-data, module @Generic.Data.Types@, and this is just one of its
-- many applications.)

-- * Testing the examples

main :: IO ()
main = do
  example Rec
  example Rec2
  example Rec3
