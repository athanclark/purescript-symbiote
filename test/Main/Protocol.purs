module Test.Main.Protocol where

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.UInt (UInt, fromInt)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Types (Uint8) as AB
import Data.ArrayBuffer.Typed.Unsafe (AV)
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength, putArrayBuffer, readArrayBuffer)
import Data.ArrayBuffer.Class.Types (Uint8 (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Time.Duration (Milliseconds (..))
import Test.Serialization.Symbiote
  ( class SymbioteOperation, Topic (..), register
  , SymbioteT, Generating, Operating, First, Second)
import Test.Serialization.Symbiote.Argonaut (ToArgonaut, ShowJson)
import Test.Serialization.Symbiote.ArrayBuffer (ToArrayBuffer)
import Test.Serialization.Symbiote.Debug (Debug (..))
import Test.Serialization.Symbiote.WebSocket (secondPeerWebSocketJson, secondPeerWebSocketArrayBuffer)
import Test.Serialization.Symbiote.ZeroMQ (secondPeerZeroMQ)
import Test.QuickCheck (class Arbitrary)
import Test.Spec (describe, it, SpecT)
import Effect.Aff (Aff, delay)
import Type.Proxy (Proxy (..))



protocolTests :: forall m'. Monad m' => SpecT Aff Unit m' Unit
protocolTests = do
  describe "WebSocket Client" do
    it "Json" $
      let tests :: SymbioteT ShowJson Aff Unit
          tests = do
            register (Topic "Generating Topic") 100 (Proxy :: Proxy { value :: ToArgonaut (Generating' Topic'), output :: ToArgonaut (Generating' Topic'), operation :: ToArgonaut GeneratingOperation })
            register (Topic "Operating Topic") 100 (Proxy :: Proxy { value :: ToArgonaut (Operating' Topic'), output :: ToArgonaut (Operating' Topic'), operation :: ToArgonaut OperatingOperation })
            register (Topic "First Topic") 100 (Proxy :: Proxy { value :: ToArgonaut (First' Topic'), output :: ToArgonaut (First' Topic'), operation :: ToArgonaut FirstOperation })
            register (Topic "Second Topic") 100 (Proxy :: Proxy { value :: ToArgonaut (Second' Topic'), output :: ToArgonaut (Second' Topic'), operation :: ToArgonaut SecondOperation })
            register (Topic "Topic") 100 (Proxy :: Proxy {value :: ToArgonaut Topic', output :: ToArgonaut Topic', operation :: ToArgonaut TopicOperation})
      in  secondPeerWebSocketJson "ws://localhost:3000/" NoDebug tests
    it "ArrayBuffer" $
      let tests :: SymbioteT (AV AB.Uint8 UInt) Aff Unit
          tests = do
            register (Topic "Generating ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (Generating' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (Generating' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer GeneratingOperation })
            register (Topic "Operating ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (Operating' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (Operating' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer OperatingOperation })
            register (Topic "First ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (First' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (First' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer FirstOperation })
            register (Topic "Second ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (Second' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (Second' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer SecondOperation })
            register (Topic "Topic") 50 (Proxy :: Proxy {value :: ToArrayBuffer Topic', output :: ToArrayBuffer Topic', operation :: ToArrayBuffer TopicOperation})
      in  do
        delay (Milliseconds 2000.0)
        secondPeerWebSocketArrayBuffer "ws://localhost:3001/" NoDebug tests
  describe "ZeroMQ Client" do
    it "ArrayBuffer" $
      let tests :: SymbioteT (AV AB.Uint8 UInt) Aff Unit
          tests = do
            register (Topic "Generating ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (Generating' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (Generating' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer GeneratingOperation })
            register (Topic "Operating ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (Operating' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (Operating' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer OperatingOperation })
            register (Topic "First ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (First' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (First' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer FirstOperation })
            register (Topic "Second ByteString") 50 (Proxy :: Proxy { value :: ToArrayBuffer (Second' (AV AB.Uint8 UInt)), output :: ToArrayBuffer (Second' (AV AB.Uint8 UInt)), operation :: ToArrayBuffer SecondOperation })
            register (Topic "Topic") 50 (Proxy :: Proxy {value :: ToArrayBuffer Topic', output :: ToArrayBuffer Topic', operation :: ToArrayBuffer TopicOperation})
      in  do
        delay (Milliseconds 2000.0)
        secondPeerZeroMQ "tcp://127.0.0.1:3002" NoDebug tests



-- internal instances
newtype Topic' = Topic' Topic
derive instance genericTopic' :: Generic Topic' _
derive newtype instance eqTopic' :: Eq Topic'
derive newtype instance showTopic' :: Show Topic'
derive newtype instance arbitraryTopic' :: Arbitrary Topic'
derive newtype instance encodeJsonTopic' :: EncodeJson Topic'
derive newtype instance decodeJsonTopic' :: DecodeJson Topic'
derive newtype instance encodeArrayBufferTopic' :: EncodeArrayBuffer Topic'
derive newtype instance decodeArrayBufferTopic' :: DecodeArrayBuffer Topic'
derive newtype instance dynamicByteLengthTopic' :: DynamicByteLength Topic'
data TopicOperation = TopicId
derive instance genericTopicOperation :: Generic TopicOperation _
instance showTopicOperation :: Show TopicOperation where
  show = genericShow
instance eqTopicOperation :: Eq TopicOperation where
  eq = genericEq
instance arbitraryTopicOperation :: Arbitrary TopicOperation where
  arbitrary = pure TopicId
instance encodeJsonTopicOperation :: EncodeJson TopicOperation where
  encodeJson TopicId = encodeJson "id"
instance decodeJsonTopicOperation :: DecodeJson TopicOperation where
  decodeJson json = do
    s <- decodeJson json
    if s == "id" then pure TopicId else Left "TopicOperation"
instance dynamicByteLengthTopicOperation :: DynamicByteLength TopicOperation where
  byteLength _ = pure 1
instance encodeArrayBufferTopicOperation :: EncodeArrayBuffer TopicOperation where
  putArrayBuffer b o TopicId = putArrayBuffer b o (Uint8 (fromInt 0))
instance decodeArrayBufferTopicOperation :: DecodeArrayBuffer TopicOperation where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i)
        | i == fromInt 0 -> pure (Just TopicId)
        | otherwise -> pure Nothing
instance symbioteOperationTopicOperation :: SymbioteOperation Topic' Topic' TopicOperation where
  perform TopicId x = x

newtype Generating' a = Generating' (Generating a)
derive instance genericGenerating' :: Generic a a' => Generic (Generating' a) _
derive newtype instance eqGenerating' :: (Generic a a', Eq a) => Eq (Generating' a)
derive newtype instance showGenerating' :: (Generic a a', Show a) => Show (Generating' a)
derive newtype instance arbitraryGenerating' :: Arbitrary a => Arbitrary (Generating' a)
derive newtype instance encodeJsonGenerating' :: EncodeJson a => EncodeJson (Generating' a)
derive newtype instance decodeJsonGenerating' :: DecodeJson a => DecodeJson (Generating' a)
derive newtype instance encodeArrayBufferGenerating' :: EncodeArrayBuffer a => EncodeArrayBuffer (Generating' a)
derive newtype instance decodeArrayBufferGenerating' :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Generating' a)
derive newtype instance dynamicByteLengthGenerating' :: DynamicByteLength a => DynamicByteLength (Generating' a)
data GeneratingOperation = GeneratingId
derive instance genericGeneratingOperation :: Generic GeneratingOperation _
instance showGeneratingOperation :: Show GeneratingOperation where
  show = genericShow
instance eqGeneratingOperation :: Eq GeneratingOperation where
  eq = genericEq
instance arbitraryGeneratingOperation :: Arbitrary GeneratingOperation where
  arbitrary = pure GeneratingId
instance encodeJsonGeneratingOperation :: EncodeJson GeneratingOperation where
  encodeJson GeneratingId = encodeJson "id"
instance decodeJsonGeneratingOperation :: DecodeJson GeneratingOperation where
  decodeJson json = do
    s <- decodeJson json
    if s == "id" then pure GeneratingId else Left "GeneratingOperation"
instance dynamicByteLengthGeneratingOperation :: DynamicByteLength GeneratingOperation where
  byteLength _ = pure 1
instance encodeArrayBufferGeneratingOperation :: EncodeArrayBuffer GeneratingOperation where
  putArrayBuffer b o GeneratingId = putArrayBuffer b o (Uint8 (fromInt 0))
instance decodeArrayBufferGeneratingOperation :: DecodeArrayBuffer GeneratingOperation where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i)
        | i == fromInt 0 -> pure (Just GeneratingId)
        | otherwise -> pure Nothing
instance symbioteOperationGeneratingOperation :: SymbioteOperation (Generating' a) (Generating' a) GeneratingOperation where
  perform GeneratingId x = x

newtype Operating' a = Operating' (Operating a)
derive instance genericOperating' :: Generic a a' => Generic (Operating' a) _
derive newtype instance eqOperating' :: (Generic a a', Eq a) => Eq (Operating' a)
derive newtype instance showOperating' :: (Generic a a', Show a) => Show (Operating' a)
derive newtype instance arbitraryOperating' :: Arbitrary a => Arbitrary (Operating' a)
derive newtype instance encodeJsonOperating' :: EncodeJson a => EncodeJson (Operating' a)
derive newtype instance decodeJsonOperating' :: DecodeJson a => DecodeJson (Operating' a)
derive newtype instance encodeArrayBufferOperating' :: EncodeArrayBuffer a => EncodeArrayBuffer (Operating' a)
derive newtype instance decodeArrayBufferOperating' :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Operating' a)
derive newtype instance dynamicByteLengthOperating' :: DynamicByteLength a => DynamicByteLength (Operating' a)
data OperatingOperation = OperatingId
derive instance genericOperatingOperation :: Generic OperatingOperation _
instance showOperatingOperation :: Show OperatingOperation where
  show = genericShow
instance eqOperatingOperation :: Eq OperatingOperation where
  eq = genericEq
instance arbitraryOperatingOperation :: Arbitrary OperatingOperation where
  arbitrary = pure OperatingId
instance encodeJsonOperatingOperation :: EncodeJson OperatingOperation where
  encodeJson OperatingId = encodeJson "id"
instance decodeJsonOperatingOperation :: DecodeJson OperatingOperation where
  decodeJson json = do
    s <- decodeJson json
    if s == "id" then pure OperatingId else Left "OperatingOperation"
instance dynamicByteLengthOperatingOperation :: DynamicByteLength OperatingOperation where
  byteLength _ = pure 1
instance encodeArrayBufferOperatingOperation :: EncodeArrayBuffer OperatingOperation where
  putArrayBuffer b o OperatingId = putArrayBuffer b o (Uint8 (fromInt 0))
instance decodeArrayBufferOperatingOperation :: DecodeArrayBuffer OperatingOperation where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i)
        | i == fromInt 0 -> pure (Just OperatingId)
        | otherwise -> pure Nothing
instance symbioteOperationOperatingOperation :: SymbioteOperation (Operating' a) (Operating' a) OperatingOperation where
  perform OperatingId x = x

newtype First' a = First' (First a)
derive instance genericFirst' :: Generic a a' => Generic (First' a) _
derive newtype instance eqFirst' :: (Generic a a', Eq a) => Eq (First' a)
derive newtype instance showFirst' :: (Generic a a', Show a) => Show (First' a)
derive newtype instance arbitraryFirst' :: Arbitrary a => Arbitrary (First' a)
derive newtype instance encodeJsonFirst' :: EncodeJson a => EncodeJson (First' a)
derive newtype instance decodeJsonFirst' :: DecodeJson a => DecodeJson (First' a)
derive newtype instance encodeArrayBufferFirst' :: EncodeArrayBuffer a => EncodeArrayBuffer (First' a)
derive newtype instance decodeArrayBufferFirst' :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (First' a)
derive newtype instance dynamicByteLengthFirst' :: DynamicByteLength a => DynamicByteLength (First' a)
data FirstOperation = FirstId
derive instance genericFirstOperation :: Generic FirstOperation _
instance showFirstOperation :: Show FirstOperation where
  show = genericShow
instance eqFirstOperation :: Eq FirstOperation where
  eq = genericEq
instance arbitraryFirstOperation :: Arbitrary FirstOperation where
  arbitrary = pure FirstId
instance encodeJsonFirstOperation :: EncodeJson FirstOperation where
  encodeJson FirstId = encodeJson "id"
instance decodeJsonFirstOperation :: DecodeJson FirstOperation where
  decodeJson json = do
    s <- decodeJson json
    if s == "id" then pure FirstId else Left "FirstOperation"
instance dynamicByteLengthFirstOperation :: DynamicByteLength FirstOperation where
  byteLength _ = pure 1
instance encodeArrayBufferFirstOperation :: EncodeArrayBuffer FirstOperation where
  putArrayBuffer b o FirstId = putArrayBuffer b o (Uint8 (fromInt 0))
instance decodeArrayBufferFirstOperation :: DecodeArrayBuffer FirstOperation where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i)
        | i == fromInt 0 -> pure (Just FirstId)
        | otherwise -> pure Nothing
instance symbioteOperationFirstOperation :: SymbioteOperation (First' a) (First' a) FirstOperation where
  perform FirstId x = x

newtype Second' a = Second' (Second a)
derive instance genericSecond' :: Generic a a' => Generic (Second' a) _
derive newtype instance eqSecond' :: (Generic a a', Eq a) => Eq (Second' a)
derive newtype instance showSecond' :: (Generic a a', Show a) => Show (Second' a)
derive newtype instance arbitrarySecond' :: Arbitrary a => Arbitrary (Second' a)
derive newtype instance encodeJsonSecond' :: EncodeJson a => EncodeJson (Second' a)
derive newtype instance decodeJsonSecond' :: DecodeJson a => DecodeJson (Second' a)
derive newtype instance encodeArrayBufferSecond' :: EncodeArrayBuffer a => EncodeArrayBuffer (Second' a)
derive newtype instance decodeArrayBufferSecond' :: (DecodeArrayBuffer a, DynamicByteLength a) => DecodeArrayBuffer (Second' a)
derive newtype instance dynamicByteLengthSecond' :: DynamicByteLength a => DynamicByteLength (Second' a)
data SecondOperation = SecondId
derive instance genericSecondOperation :: Generic SecondOperation _
instance showSecondOperation :: Show SecondOperation where
  show = genericShow
instance eqSecondOperation :: Eq SecondOperation where
  eq = genericEq
instance arbitrarySecondOperation :: Arbitrary SecondOperation where
  arbitrary = pure SecondId
instance encodeJsonSecondOperation :: EncodeJson SecondOperation where
  encodeJson SecondId = encodeJson "id"
instance decodeJsonSecondOperation :: DecodeJson SecondOperation where
  decodeJson json = do
    s <- decodeJson json
    if s == "id" then pure SecondId else Left "SecondOperation"
instance dynamicByteLengthSecondOperation :: DynamicByteLength SecondOperation where
  byteLength _ = pure 1
instance encodeArrayBufferSecondOperation :: EncodeArrayBuffer SecondOperation where
  putArrayBuffer b o SecondId = putArrayBuffer b o (Uint8 (fromInt 0))
instance decodeArrayBufferSecondOperation :: DecodeArrayBuffer SecondOperation where
  readArrayBuffer b o = do
    mX <- readArrayBuffer b o
    case mX of
      Nothing -> pure Nothing
      Just (Uint8 i)
        | i == fromInt 0 -> pure (Just SecondId)
        | otherwise -> pure Nothing
instance symbioteOperationSecondOperation :: SymbioteOperation (Second' a) (Second' a) SecondOperation where
  perform SecondId x = x
