module Test.Serialization.Symbiote.Argonaut where

import Prelude
import Test.Serialization.Symbiote (class SymbioteOperation, class Symbiote, perform)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, encodeJson, decodeJson, stringify, Json) as Json
import Test.QuickCheck (class Arbitrary)


newtype ToArgonaut a = ToArgonaut a
derive instance genericToArgonaut :: Generic a a' => Generic (ToArgonaut a) _
derive newtype instance arbitraryToArgonaut :: Arbitrary a => Arbitrary (ToArgonaut a)
derive newtype instance eqToArgonaut :: Eq a => Eq (ToArgonaut a)
derive newtype instance showToArgonaut :: Show a => Show (ToArgonaut a)

instance symbioteOperationToArgonaut :: SymbioteOperation a op => SymbioteOperation (ToArgonaut a) (ToArgonaut op) where
  perform (ToArgonaut x) (ToArgonaut y) = ToArgonaut (perform x y)

instance symbioteToArgonaut ::
  ( SymbioteOperation (ToArgonaut a) (ToArgonaut op)
  , Json.EncodeJson a
  , Json.EncodeJson op
  , Json.DecodeJson a
  , Json.DecodeJson op
  ) => Symbiote (ToArgonaut a) (ToArgonaut op) ShowJson where
  encode (ToArgonaut x) = ShowJson (Json.encodeJson x)
  decode (ShowJson x) = case Json.decodeJson x of
    Left _ -> Nothing
    Right y -> Just (ToArgonaut y)
  encodeOp (ToArgonaut x) = ShowJson (Json.encodeJson x)
  decodeOp (ShowJson x) = case Json.decodeJson x of
    Left _ -> Nothing
    Right y -> Just (ToArgonaut y)


-- | Simple newtype to make the serialized output `Show`able
newtype ShowJson = ShowJson Json.Json
derive instance genericShowJson :: Generic ShowJson _
derive newtype instance eqShowJson :: Eq ShowJson
instance showShowJson :: Show ShowJson where
  show (ShowJson x) = Json.stringify x
