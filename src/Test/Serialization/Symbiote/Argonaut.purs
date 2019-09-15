module Test.Serialization.Symbiote.Argonaut where

import Test.Serialization.Symbiote (class SymbioteOperation, class Symbiote, perform)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, encodeJson, decodeJson) as Json


newtype ToArgonaut a = ToArgonaut a
derive instance genericToArgonaut :: Generic a a' => Generic (ToArgonaut a) _

instance symbioteOperationToArgonaut :: SymbioteOperation a op => SymbioteOperation (ToArgonaut a) (ToArgonaut op) where
  perform (ToArgonaut x) (ToArgonaut y) = ToArgonaut (perform x y)

instance symbioteToArgonaut ::
  ( SymbioteOperation (ToArgonaut a) (ToArgonaut op)
  , Json.EncodeJson a
  , Json.EncodeJson op
  , Json.DecodeJson a
  , Json.DecodeJson op
  ) => Symbiote (ToArgonaut a) (ToArgonaut op) Json.Json where
  encode (ToArgonaut x) = Json.encodeJson x
  decode x = case Json.decodeJson x of
    Left _ -> Nothing
    Right y -> Just (ToArgonaut y)
  encodeOp (ToArgonaut x) = Json.encodeJson x
  decodeOp x = case Json.decodeJson x of
    Left _ -> Nothing
    Right y -> Just (ToArgonaut y)
