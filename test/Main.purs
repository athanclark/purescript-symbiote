module Test.Main where

import Test.Serialization.Symbiote
  ( class SymbioteOperation, class Symbiote, Topic (..), register
  , perform, SymbioteT, SimpleSerialization, simpleTest, Generating, Operating, First, Second)
import Test.Serialization.Symbiote.Argonaut (ToArgonaut, ShowJson)
import Test.Serialization.Symbiote.ArrayBuffer (ToArrayBuffer)
import Test.Serialization.Symbiote.Abides
import Test.Serialization.Symbiote.WebSocket (secondPeerWebSocketJson, Debug (..))

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson, Json, (.:), (:=), jsonEmptyObject, (~>)
  )
import Data.Argonaut
  (fromBoolean, jsonNull, fromNumber, fromString, fromArray, fromObject, stringify, jsonParser) as Json
import Data.NonEmpty (NonEmpty (..))
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Array (reverse, init, tail) as Array
import Data.ArrayBuffer.Types (Uint8)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength
  , encodeArrayBuffer, decodeArrayBuffer, byteLength, putArrayBuffer, readArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int32BE (..), Float64BE (..), Int8 (..))
import Data.ArrayBuffer.Typed (whole, buffer)
import Data.ArrayBuffer.Typed.Unsafe (AV (..))
import Data.UInt (UInt)
import Control.Alternative ((<|>))
import Foreign.Object (fromFoldable) as Object
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import Test.Spec (describe, it)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.QuickCheck (class Arbitrary, arbitrary, quickCheck)
import Test.QuickCheck.Gen (Gen, oneOf, elements, sized, resize, arrayOf)
import Type.Proxy (Proxy (..))


main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig {timeout = Nothing}) [consoleReporter] do
  describe "Symbiote Sanity Checks" do
    simpleTests
    arraybufferTests
    jsonTests
  describe "Local Isomorphisms" do
    describe "Json" do
      let go :: forall a. Arbitrary a => EncodeJson a => DecodeJson a => Eq a => String -> Proxy a -> _
          go n p = it n (liftEffect (quickCheck (jsonIso p)))
      describe "Abides" do
        go "AbidesSemigroup (Array' Int')" (Proxy :: Proxy (AbidesSemigroup (Array' Int')))
        go "AbidesMonoid (Array' Int')" (Proxy :: Proxy (AbidesMonoid (Array' Int')))
        go "AbidesEq Int'" (Proxy :: Proxy (AbidesEq Int'))
        go "AbidesOrd Int'" (Proxy :: Proxy (AbidesOrd Int'))
        go "AbidesEnum Int'" (Proxy :: Proxy (AbidesEnum Int'))
        go "AbidesSemiring Int'" (Proxy :: Proxy (AbidesSemiring Int'))
        go "AbidesRing Int'" (Proxy :: Proxy (AbidesRing Int'))
        go "AbidesCommutativeRing Int'" (Proxy :: Proxy (AbidesCommutativeRing Int'))
        go "AbidesDivisionRing Int'" (Proxy :: Proxy (AbidesDivisionRing Int'))
        go "AbidesEuclideanRing Int'" (Proxy :: Proxy (AbidesEuclideanRing Int'))
        go "AbidesField Int'" (Proxy :: Proxy (AbidesField Int'))
      describe "Symbiote" do
        go "Generating Int'" (Proxy :: Proxy (Generating Int'))
        go "Operating Int'" (Proxy :: Proxy (Operating Int'))
        go "First Int'" (Proxy :: Proxy (First Int'))
        go "Second Int'" (Proxy :: Proxy (Second Int'))
        go "Topic" (Proxy :: Proxy Topic)
    describe "ArrayBuffer" do
      let go :: forall a. Arbitrary a => EncodeArrayBuffer a => DecodeArrayBuffer a => DynamicByteLength a => Eq a => String -> Proxy a -> _
          go n p = it n (liftEffect (quickCheck (cerealIso p)))
      describe "Abides" do
        go "AbidesSemigroup (Array' Int')" (Proxy :: Proxy (AbidesSemigroup (Array' Int')))
        go "AbidesMonoid (Array' Int')" (Proxy :: Proxy (AbidesMonoid (Array' Int')))
        go "AbidesEq Int'" (Proxy :: Proxy (AbidesEq Int'))
        go "AbidesOrd Int'" (Proxy :: Proxy (AbidesOrd Int'))
        go "AbidesEnum Int'" (Proxy :: Proxy (AbidesEnum Int'))
        go "AbidesSemiring Int'" (Proxy :: Proxy (AbidesSemiring Int'))
        go "AbidesRing Int'" (Proxy :: Proxy (AbidesRing Int'))
        go "AbidesCommutativeRing Int'" (Proxy :: Proxy (AbidesCommutativeRing Int'))
        go "AbidesDivisionRing Int'" (Proxy :: Proxy (AbidesDivisionRing Int'))
        go "AbidesEuclideanRing Int'" (Proxy :: Proxy (AbidesEuclideanRing Int'))
        go "AbidesField Int'" (Proxy :: Proxy (AbidesField Int'))
      describe "Symbiote" do
        go "Generating Int'" (Proxy :: Proxy (Generating Int'))
        go "Operating Int'" (Proxy :: Proxy (Operating Int'))
        go "First Int'" (Proxy :: Proxy (First Int'))
        go "Second Int'" (Proxy :: Proxy (Second Int'))
        go "Topic" (Proxy :: Proxy Topic)
  describe "WebSocket Client" do
    it "Json" $
      let tests :: SymbioteT ShowJson Aff Unit
          tests = do
            -- register (Topic "Generating Boolean") 100 (Proxy :: Proxy (Generating Boolean))
            -- register (Topic "Operating Boolean") 100 (Proxy :: Proxy (Operating Boolean))
            -- register (Topic "First Boolean") 100 (Proxy :: Proxy (First Boolean))
            -- register (Topic "Second Boolean") 100 (Proxy :: Proxy (Second Boolean))
            register (Topic "Topic") 100 (Proxy :: Proxy {value :: ToArgonaut Topic', output :: ToArgonaut Topic', operation :: ToArgonaut TopicOperation})
      in  secondPeerWebSocketJson "ws://localhost:3000/" Debug tests
  where
    simpleTests = describe "Simple Tests" do
      it "Unit over id" (simpleTest unitSuite)
      it "Int over various" (simpleTest intSuite)
      it "Number over various" (simpleTest numberSuite)
      it "Array over various" (simpleTest arraySuite)
      where
        unitSuite :: SymbioteT (SimpleSerialization Unit' Unit' Unit'Operation) Aff Unit
        unitSuite = register (Topic "Unit") 100
          (Proxy :: Proxy {value :: Unit', output :: Unit', operation :: Unit'Operation})
        intSuite :: SymbioteT (SimpleSerialization Int' Boolean Int'Operation) Aff Unit
        intSuite = register (Topic "Int") 100
          (Proxy :: Proxy {value :: Int', output :: Boolean, operation :: Int'Operation})
        numberSuite :: SymbioteT (SimpleSerialization Number' Boolean Number'Operation) Aff Unit
        numberSuite = register (Topic "Number") 100
          (Proxy :: Proxy {value :: Number', output :: Boolean, operation :: Number'Operation})
        arraySuite :: SymbioteT (SimpleSerialization (Array' Int') (Either Boolean (Array' Int')) (Array'Operation Int')) Aff Unit
        arraySuite = register (Topic "Array") 100
          (Proxy :: Proxy {value :: Array' Int', output :: Either Boolean (Array' Int'), operation :: (Array'Operation Int')})
    arraybufferTests = describe "ArrayBuffer Tests" do
      it "Json over id" (simpleTest jsonSuite)
      it "Int over various" (simpleTest intSuite)
      it "Number over various" (simpleTest numberSuite)
      it "Array over various" (simpleTest arraySuite)
      where
        jsonSuite :: SymbioteT (AV Uint8 UInt) Aff Unit
        jsonSuite = register (Topic "Json") 100
          (Proxy :: Proxy {value :: Json', output :: Json', operation :: Json'Operation})
        intSuite :: SymbioteT (AV Uint8 UInt) Aff Unit
        intSuite = register (Topic "Int") 100
          (Proxy :: Proxy {value :: ToArrayBuffer Int', output :: ToArrayBuffer Boolean, operation :: ToArrayBuffer Int'Operation})
        numberSuite :: SymbioteT (AV Uint8 UInt) Aff Unit
        numberSuite = register (Topic "Number") 100
          (Proxy :: Proxy {value :: ToArrayBuffer Number', output :: ToArrayBuffer Boolean, operation :: ToArrayBuffer Number'Operation})
        arraySuite :: SymbioteT (AV Uint8 UInt) Aff Unit
        arraySuite = register (Topic "Array") 100
          (Proxy :: Proxy {value :: ToArrayBuffer (Array' Int'), output :: ToArrayBuffer (Either Boolean (Array' Int')), operation :: ToArrayBuffer (Array'Operation Int')})
    jsonTests = describe "Json Tests" do
      it "Int over various" (simpleTest intSuite)
      it "Number over various" (simpleTest numberSuite)
      it "Array over various" (simpleTest arraySuite)
      where
        intSuite :: SymbioteT ShowJson Aff Unit
        intSuite = register (Topic "Int") 100
          (Proxy :: Proxy {value :: ToArgonaut Int', output :: ToArgonaut Boolean, operation :: ToArgonaut Int'Operation})
        numberSuite :: SymbioteT ShowJson Aff Unit
        numberSuite = register (Topic "Number") 100
          (Proxy :: Proxy {value :: ToArgonaut Number', output :: ToArgonaut Boolean, operation :: ToArgonaut Number'Operation})
        arraySuite :: SymbioteT ShowJson Aff Unit
        arraySuite = register (Topic "Array") 100
          (Proxy :: Proxy {value :: ToArgonaut (Array' Int'), output :: ToArgonaut (Either Boolean (Array' Int')), operation :: ToArgonaut (Array'Operation Int')})



data Unit' = Unit'
derive instance genericUnit' :: Generic Unit' _
instance showUnit' :: Show Unit' where
  show = genericShow
instance eqUnit' :: Eq Unit' where
  eq = genericEq
instance arbitraryUnit' :: Arbitrary Unit' where
  arbitrary = pure Unit'
instance encodeJsonUnit' :: EncodeJson Unit' where
  encodeJson Unit' = encodeJson unit
instance decodeJsonUnit' :: DecodeJson Unit' where
  decodeJson x = do
    (_ :: Unit) <- decodeJson x
    pure Unit'
data Unit'Operation = IdUnit'
derive instance genericUnit'Operation :: Generic Unit'Operation _
instance showUnit'Operation :: Show Unit'Operation where
  show = genericShow
instance eqUnit'Operation :: Eq Unit'Operation where
  eq = genericEq
instance arbitraryUnit'Operation :: Arbitrary Unit'Operation where
  arbitrary = pure IdUnit'
instance encodeJsonUnit'Operation :: EncodeJson Unit'Operation where
  encodeJson IdUnit' = encodeJson unit
instance decodeJsonUnit'Operation :: DecodeJson Unit'Operation where
  decodeJson x = do
    (_ :: Unit) <- decodeJson x
    pure IdUnit'
instance symbioteOperationUnit' :: SymbioteOperation Unit' Unit' Unit'Operation where
  perform IdUnit' x = x


newtype Int' = Int' Int
derive instance genericInt' :: Generic Int' _
derive newtype instance showInt' :: Show Int'
derive newtype instance eqInt' :: Eq Int'
derive newtype instance semiringInt' :: Semiring Int'
derive newtype instance ringInt' :: Ring Int'
derive newtype instance commutativeRingInt' :: CommutativeRing Int'
derive newtype instance euclideanRingInt' :: EuclideanRing Int'
derive newtype instance arbitraryInt' :: Arbitrary Int'
derive newtype instance encodeJsonInt' :: EncodeJson Int'
derive newtype instance decodeJsonInt' :: DecodeJson Int'
instance dynamicByteLengthInt' :: DynamicByteLength Int' where
  byteLength (Int' x) = byteLength (Int32BE x)
instance encodeArrayBufferInt' :: EncodeArrayBuffer Int' where
  putArrayBuffer b o (Int' x) = putArrayBuffer b o (Int32BE x)
instance decodeArrayBufferInt' :: DecodeArrayBuffer Int' where
  readArrayBuffer b o = do
    mx <- readArrayBuffer b o
    case mx of
      Nothing -> pure Nothing
      Just (Int32BE x) -> pure (Just (Int' x))
newtype Int'Operation = Int'Operation (AbidesEuclideanRingOperation Int')
derive instance genericInt'Operation :: Generic Int'Operation _
derive newtype instance showInt'Operation :: Show Int'Operation
derive newtype instance eqInt'Operation :: Eq Int'Operation
derive newtype instance arbitraryInt'Operation :: Arbitrary Int'Operation
derive newtype instance encodeJsonInt'Operation :: EncodeJson Int'Operation
derive newtype instance decodeJsonInt'Operation :: DecodeJson Int'Operation
derive newtype instance dynamicByteLengthInt'Operation :: DynamicByteLength Int'Operation
derive newtype instance encodeArrayBufferInt'Operation :: EncodeArrayBuffer Int'Operation
derive newtype instance decodeArrayBufferInt'Operation :: DecodeArrayBuffer Int'Operation
instance symbioteOperationInt' :: SymbioteOperation Int' Boolean Int'Operation where
  perform op x = case op of
    Int'Operation op' -> perform op' (AbidesEuclideanRing x)


newtype Number' = Number' Number
derive instance genericNumber' :: Generic Number' _
derive newtype instance showNumber' :: Show Number'
derive newtype instance eqNumber' :: Eq Number'
derive newtype instance semiringNumber' :: Semiring Number'
derive newtype instance ringNumber' :: Ring Number'
derive newtype instance commutativeRingNumber' :: CommutativeRing Number'
derive newtype instance divisionRingNumber' :: DivisionRing Number'
derive newtype instance euclideanRingNumber' :: EuclideanRing Number'
derive newtype instance arbitraryNumber' :: Arbitrary Number'
derive newtype instance encodeJsonNumber' :: EncodeJson Number'
derive newtype instance decodeJsonNumber' :: DecodeJson Number'
instance dynamicByteLengthNumber' :: DynamicByteLength Number' where
  byteLength (Number' x) = byteLength (Float64BE x)
instance encodeArrayBufferNumber' :: EncodeArrayBuffer Number' where
  putArrayBuffer b o (Number' x) = putArrayBuffer b o (Float64BE x)
instance decodeArrayBufferNumber' :: DecodeArrayBuffer Number' where
  readArrayBuffer b o = do
    mx <- readArrayBuffer b o
    case mx of
      Nothing -> pure Nothing
      Just (Float64BE x) -> pure (Just (Number' x))
newtype Number'Operation = Number'Operation (AbidesFieldOperation Number')
derive instance genericNumber'Operation :: Generic Number'Operation _
derive newtype instance showNumber'Operation :: Show Number'Operation
derive newtype instance eqNumber'Operation :: Eq Number'Operation
derive newtype instance arbitraryNumber'Operation :: Arbitrary Number'Operation
derive newtype instance encodeJsonNumber'Operation :: EncodeJson Number'Operation
derive newtype instance decodeJsonNumber'Operation :: DecodeJson Number'Operation
derive newtype instance dynamicByteLengthNumber'Operation :: DynamicByteLength Number'Operation
derive newtype instance encodeArrayBufferNumber'Operation :: EncodeArrayBuffer Number'Operation
derive newtype instance decodeArrayBufferNumber'Operation :: DecodeArrayBuffer Number'Operation
instance symbioteOperationNumber' :: SymbioteOperation Number' Boolean Number'Operation where
  perform op x = case op of
    Number'Operation op' -> perform op' (AbidesField x)


newtype Array' a = Array' (Array a)
derive instance genericArray' :: Generic a a' => Generic (Array' a) _
derive newtype instance showArray' :: Show a => Show (Array' a)
derive newtype instance eqArray' :: Eq a => Eq (Array' a)
derive newtype instance semigroupArray' :: Semigroup (Array' a)
derive newtype instance monoidArray' :: Monoid (Array' a)
derive newtype instance arbitraryArray' :: Arbitrary a => Arbitrary (Array' a)
derive newtype instance encodeJsonArray' :: EncodeJson a => EncodeJson (Array' a)
derive newtype instance decodeJsonArray' :: DecodeJson a => DecodeJson (Array' a)
derive newtype instance dynamicByteLengthArray' :: DynamicByteLength a => DynamicByteLength (Array' a)
derive newtype instance encodeArrayBufferArray' :: EncodeArrayBuffer a => EncodeArrayBuffer (Array' a)
derive newtype instance decodeArrayBufferArray' :: (DynamicByteLength a, DecodeArrayBuffer a) => DecodeArrayBuffer (Array' a)
data Array'Operation a
  = ReverseArray
  | InitArray
  | TailArray
  | Array'Monoid (AbidesMonoidOperation (Array' a))
derive instance genericArray'Operation :: Generic a a' => Generic (Array'Operation a) _
instance showArray'Operation :: (Show a, Generic a a') => Show (Array'Operation a) where
  show = genericShow
instance eqArray'Operation :: (Eq a, Generic a a') => Eq (Array'Operation a) where
  eq = genericEq
instance arbitraryArray'Operation :: Arbitrary a => Arbitrary (Array'Operation a) where
  arbitrary = oneOf $ NonEmpty (pure ReverseArray)
    [ pure InitArray
    , pure TailArray
    , Array'Monoid <$> arbitrary
    ]
instance encodeJsonArray'Operation :: EncodeJson a => EncodeJson (Array'Operation a) where
  encodeJson x = case x of
    ReverseArray -> encodeJson "reverse"
    InitArray -> encodeJson "init"
    TailArray -> encodeJson "tail"
    Array'Monoid op -> "monoid" := op ~> jsonEmptyObject
instance decodeJsonArray'Operation :: DecodeJson a => DecodeJson (Array'Operation a) where
  decodeJson x = object <|> string
    where
      string = do
        s <- decodeJson x
        case unit of
          _ | s == "reverse" -> pure ReverseArray
            | s == "init" -> pure InitArray
            | s == "tail" -> pure TailArray
            | otherwise -> Left "Not a Array"
      object = do
        o <- decodeJson x
        Array'Monoid <$> o .: "monoid"
instance dynamicByteLengthArray'Operation :: DynamicByteLength a => DynamicByteLength (Array'Operation a) where
  byteLength op = case op of
    Array'Monoid op' -> (\l -> l + 1) <$> byteLength op'
    _ -> pure 1
instance encodeArrayBufferArray'Operation :: EncodeArrayBuffer a => EncodeArrayBuffer (Array'Operation a) where
  putArrayBuffer b o op = case op of
    ReverseArray -> putArrayBuffer b o $ Int8 0
    InitArray -> putArrayBuffer b o $ Int8 1
    TailArray -> putArrayBuffer b o $ Int8 2
    Array'Monoid op' -> do
      mL <- putArrayBuffer b o $ Int8 3
      case mL of
        Nothing -> pure Nothing
        Just l -> do
          mL' <- putArrayBuffer b (o + l) op'
          case mL' of
            Nothing -> pure (Just l)
            Just l' -> pure (Just (l + l'))
instance decodeArrayBufferArray'Operation :: (DynamicByteLength a, DecodeArrayBuffer a) => DecodeArrayBuffer (Array'Operation a) where
  readArrayBuffer b o = do
    mx <- readArrayBuffer b o
    case mx of
      Nothing -> pure Nothing
      Just (Int8 x)
        | x == 0 -> pure (Just ReverseArray)
        | x == 1 -> pure (Just InitArray)
        | x == 2 -> pure (Just TailArray)
        | x == 3 -> do
          mOp <- readArrayBuffer b (o + 1)
          case mOp of
            Nothing -> pure Nothing
            Just op -> pure (Just (Array'Monoid op))
        | otherwise -> pure Nothing
instance symbioteOperationArray' :: (Eq a) => SymbioteOperation (Array' a) (Either Boolean (Array' a)) (Array'Operation a) where
  perform op x'@(Array' x) = case op of
    ReverseArray -> Right $ Array' $ Array.reverse x
    InitArray -> Right $ Array' $ case Array.init x of
      Nothing -> []
      Just y -> y
    TailArray -> Right $ Array' $ case Array.tail x of
      Nothing -> []
      Just y -> y
    Array'Monoid op' -> Left $ perform op' $ AbidesMonoid x'


newtype Json' = Json' Json
derive instance genericJson' :: Generic Json' _
instance showJson' :: Show Json' where
  show (Json' x) = Json.stringify x
derive newtype instance eqJson' :: Eq Json'
instance arbitraryJson' :: Arbitrary Json' where
  arbitrary = map Json' go
    where
      go :: Gen Json
      go = sized \s ->
        if s <= 1
          then oneOf $ NonEmpty
                (pure Json.jsonNull)
                [ Json.fromBoolean <$> arbitrary
                , Json.fromNumber <$> arbitrary
                , Json.fromString <$> arbitrary
                ]
          else
            let go' = resize (s `div` 10) go
            in  (Json.fromArray <$> (arrayOf go'))
                  <|> (Json.fromObject <$> (Object.fromFoldable <$> arrayOf (Tuple <$> arbitrary <*> go')))
data Json'Operation = IdJson'
derive instance genericJson'Operation :: Generic Json'Operation _
instance showJson'Operation :: Show Json'Operation where
  show = genericShow
instance eqJson'Operation :: Eq Json'Operation where
  eq = genericEq
instance arbitraryJson'Operation :: Arbitrary Json'Operation where
  arbitrary = pure IdJson'
instance symbioteOperationJson' :: SymbioteOperation Json' Json' Json'Operation where
  perform IdJson' x = x
instance symbioteJson' :: Symbiote Json' Json' Json'Operation (AV Uint8 UInt) where
  encode (Json' x) = unsafePerformEffect do
    b <- encodeArrayBuffer (Json.stringify x)
    AV <$> whole b
  decode (AV t) = unsafePerformEffect do
    ms <- decodeArrayBuffer (buffer t)
    case Json.jsonParser <$> ms of
      Nothing -> Nothing <$ error "No buffer"
      Just me -> case me of
        Left e -> Nothing <$ error e
        Right y -> pure (Just (Json' y))
  encodeOut _ (Json' x) = unsafePerformEffect do
    b <- encodeArrayBuffer (Json.stringify x)
    AV <$> whole b
  decodeOut _ (AV t) = unsafePerformEffect do
    ms <- decodeArrayBuffer (buffer t)
    case Json.jsonParser <$> ms of
      Nothing -> Nothing <$ error "No buffer"
      Just me -> case me of
        Left e -> Nothing <$ error e
        Right y -> pure (Just (Json' y))
  encodeOp IdJson' = unsafePerformEffect do
    b <- encodeArrayBuffer "id"
    AV <$> whole b
  decodeOp (AV t) = unsafePerformEffect do
    ms <- decodeArrayBuffer (buffer t)
    case ms of
      Nothing -> pure Nothing
      Just s
        | s == "id" -> pure (Just IdJson')
        | otherwise -> pure Nothing



jsonIso :: forall a
         . EncodeJson a
        => DecodeJson a
        => Eq a => Proxy a -> a -> Boolean
jsonIso Proxy x = decodeJson (encodeJson x) == Right x


cerealIso :: forall a
           . EncodeArrayBuffer a
          => DecodeArrayBuffer a
          => DynamicByteLength a
          => Eq a
          => Proxy a -> a -> Boolean
cerealIso Proxy x = unsafePerformEffect do
  buf <- encodeArrayBuffer x
  mY <- decodeArrayBuffer buf
  pure (mY == Just x)



-- internal instances
newtype Topic' = Topic' Topic
derive instance genericTopic' :: Generic Topic' _
derive newtype instance eqTopic' :: Eq Topic'
derive newtype instance showTopic' :: Show Topic'
derive newtype instance arbitraryTopic' :: Arbitrary Topic'
derive newtype instance encodeJsonTopic' :: EncodeJson Topic'
derive newtype instance decodeJsonTopic' :: DecodeJson Topic'
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
instance symbioteOperationTopicOperation :: SymbioteOperation Topic' Topic' TopicOperation where
  perform TopicId x = x
