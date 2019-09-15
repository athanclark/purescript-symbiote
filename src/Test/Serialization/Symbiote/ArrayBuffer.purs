module Test.Serialization.Symbiote.ArrayBuffer where

import Prelude ((<$>))
import Test.Serialization.Symbiote (class SymbioteOperation, class Symbiote, perform)
import Data.Generic.Rep (class Generic)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , encodeArrayBuffer, decodeArrayBuffer) as AB
import Effect.Unsafe (unsafePerformEffect)


newtype ToArrayBuffer a = ToArrayBuffer a
derive instance genericToArrayBuffer :: Generic a a' => Generic (ToArrayBuffer a) _

instance symbioteOperationToArrayBuffer :: SymbioteOperation a op => SymbioteOperation (ToArrayBuffer a) (ToArrayBuffer op) where
  perform (ToArrayBuffer x) (ToArrayBuffer y) = ToArrayBuffer (perform x y)

instance symbioteToArrayBuffer ::
  ( SymbioteOperation (ToArrayBuffer a) (ToArrayBuffer op)
  , AB.EncodeArrayBuffer a
  , AB.EncodeArrayBuffer op
  , AB.DecodeArrayBuffer a
  , AB.DecodeArrayBuffer op
  , AB.DynamicByteLength a
  , AB.DynamicByteLength op
  ) => Symbiote (ToArrayBuffer a) (ToArrayBuffer op) ArrayBuffer where
  encode (ToArrayBuffer x) = unsafePerformEffect (AB.encodeArrayBuffer x)
  decode x = ToArrayBuffer <$> (unsafePerformEffect (AB.decodeArrayBuffer x))
  encodeOp (ToArrayBuffer x) = unsafePerformEffect (AB.encodeArrayBuffer x)
  decodeOp x = ToArrayBuffer <$> (unsafePerformEffect (AB.decodeArrayBuffer x))
