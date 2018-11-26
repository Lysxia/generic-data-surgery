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
import Generic.Data.Surgery (toOR, fromOR, toOR', fromOR', insertRField, removeRField)

-- Example: Foo encoded as a JSON object with an extra "checksum" key.
--
-- { "x": X
-- , "y": Y
-- , "z": Z
-- , "checksum": X + Y + Z
-- }

data Foo = Foo { x, y, z :: Int }
  deriving (Eq, Generic, Show)

checksum :: Foo -> Int
checksum f = x f + y f + z f

instance FromJSON Foo where
  parseJSON v = do
    r <- genericParseJSON defaultOptions v
    let (cs, f) = (fmap fromOR . removeRField @"checksum" @3 . toOR') r
    if checksum f == cs then
      pure f
    else
      fail "Checksum failed"

instance ToJSON Foo where
  toJSON f =
    (genericToJSON defaultOptions . fromOR' . insertRField @"checksum" @3 . fmap toOR)
      (checksum f, f)

assertEqual :: (Eq a, Show a) => a -> a -> IO ()
assertEqual a b
  | a == b = pure ()
  | otherwise = do
    putStrLn $ "Expected: " ++ show a
    putStrLn $ "Actual:   " ++ show b
    fail "Assertion failed, not equal."

main :: IO ()
main = do
  let ex1 = "{\"x\":0,\"y\":1,\"z\":2,\"checksum\":3}"  -- OK
      ex2 = "{\"x\":0,\"y\":1,\"z\":2,\"checksum\":4}"  -- Bad checksum

  -- Test parseJSON
  assertEqual (Right (Foo 0 1 2))
    (eitherDecode ex1)
  assertEqual (Left "Error in $: Checksum failed" :: Either String Foo)
    (eitherDecode ex2)

  -- Test toJSON
  assertEqual (eitherDecode ex1 :: Either String Value)
    (Right (toJSON (Foo 0 1 2)))
