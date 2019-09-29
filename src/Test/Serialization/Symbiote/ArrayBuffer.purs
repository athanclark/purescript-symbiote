module Test.Serialization.Symbiote.ArrayBuffer where

import Prelude
import Test.Serialization.Symbiote (class SymbioteOperation, class Symbiote, perform)
import Data.Generic.Rep (class Generic)
import Data.UInt (UInt)
import Data.ArrayBuffer.Types (Uint8)
import Data.ArrayBuffer.Typed (whole, buffer)
import Data.ArrayBuffer.Typed.Unsafe (AV (..))
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , encodeArrayBuffer, decodeArrayBuffer) as AB
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary)

-- | Wrap your subject-matter type and operations type with this, to get a system that serializes to
-- | `(AV Uint8 UInt)` - a typed version of an `ArrayBuffer`.
newtype ToArrayBuffer a = ToArrayBuffer a
derive instance genericToArrayBuffer :: Generic a a' => Generic (ToArrayBuffer a) _
derive newtype instance arbitraryToArrayBuffer :: Arbitrary a => Arbitrary (ToArrayBuffer a)
derive newtype instance eqToArrayBuffer :: Eq a => Eq (ToArrayBuffer a)
derive newtype instance showToArrayBuffer :: Show a => Show (ToArrayBuffer a)

instance symbioteOperationToArrayBuffer :: SymbioteOperation a o op => SymbioteOperation (ToArrayBuffer a) (ToArrayBuffer o) (ToArrayBuffer op) where
  perform (ToArrayBuffer x) (ToArrayBuffer y) = ToArrayBuffer (perform x y)

instance symbioteToArrayBuffer ::
  ( SymbioteOperation (ToArrayBuffer a) (ToArrayBuffer o) (ToArrayBuffer op)
  , AB.EncodeArrayBuffer a
  , AB.EncodeArrayBuffer o
  , AB.EncodeArrayBuffer op
  , AB.DecodeArrayBuffer a
  , AB.DecodeArrayBuffer o
  , AB.DecodeArrayBuffer op
  , AB.DynamicByteLength a
  , AB.DynamicByteLength o
  , AB.DynamicByteLength op
  ) => Symbiote (ToArrayBuffer a) (ToArrayBuffer o) (ToArrayBuffer op) (AV Uint8 UInt) where
  encode (ToArrayBuffer x) = unsafePerformEffect do
    b <- AB.encodeArrayBuffer x
    AV <$> whole b
  decode (AV t) = ToArrayBuffer <$> (unsafePerformEffect (AB.decodeArrayBuffer (buffer t)))
  encodeOut _ (ToArrayBuffer x) = unsafePerformEffect do
    b <- AB.encodeArrayBuffer x
    AV <$> whole b
  decodeOut _ (AV t) = ToArrayBuffer <$> (unsafePerformEffect (AB.decodeArrayBuffer (buffer t)))
  encodeOp (ToArrayBuffer x) = unsafePerformEffect do
    b <- AB.encodeArrayBuffer x
    AV <$> whole b
  decodeOp (AV t) = ToArrayBuffer <$> (unsafePerformEffect (AB.decodeArrayBuffer (buffer t)))
