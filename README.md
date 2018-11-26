# Surgery for generic data types [![Hackage](https://img.shields.io/hackage/v/generic-data-surgery.svg)](https://hackage.haskell.org/package/generic-data-surgery) [![Build Status](https://travis-ci.org/Lysxia/generic-data-surgery.svg)](https://travis-ci.org/Lysxia/generic-data-surgery)

Modify, add, or remove constructors and fields in generic types, to be used
with generic implementations.

## Example

Here is a simple record type equipped with a `checksum` function:

```haskell
data Foo = Foo { x, y, z :: Int }
  deriving (Eq, Generic, Show)

checksum :: Foo -> Checksum
```

Let's encode it as a JSON object with an extra `"checksum"` key,
looking like this, where `X`, `Y`, `Z` are integers:

```
{ "x": X
, "y": Y
, "z": Z
, "checksum": X + Y + Z
}
```

We use `genericParseJSON`/`genericToJSON` to convert between JSON values
and a generic 4-field record, and `removeRField`/`insertRField` to
convert between that generic 4-field record and the 3-field `Foo`.

### Remove field

When decoding, we check the checksum and then throw it away.

```haskell
instance FromJSON Foo where
  parseJSON v = do

    r <- genericParseJSON defaultOptions v
    -- r: a generic 4-field record {x,y,z,checksum} (checksum at index 3).

    let (cs, f) = (fmap fromOR . removeRField @"checksum" @3 . toOR') r
    -- removeRField @"checksum" @3: split out the checksum field
    -- from the three other fields. (cs, f) :: (Checksum, Foo)

    if checksum f == cs then
      pure f
    else
      fail "Checksum failed"
```

### Insert field

When encoding, we must compute the checksum to write it out. We put the
checksum in a pair `(checksum f, f)` with the original record, and
`insertRField` can then wrap it into a 4-field record passed into
`genericToJSON`.

```haskell
instance ToJSON Foo where
  toJSON f =
    (genericToJSON defaultOptions . fromOR' . insertRField @"checksum" @3 . fmap toOR)
      (checksum f, f)
```
