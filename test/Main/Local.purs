module Test.Main.Local where

import Test.Main.Types (Int', Array')
import Test.Serialization.Symbiote (Generating, Operating, First, Second, Topic)
import Test.Serialization.Symbiote.Abides

import Prelude
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength, encodeArrayBuffer, decodeArrayBuffer)
import Test.Spec (describe, it, SpecT)
import Test.QuickCheck (class Arbitrary, quickCheck)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Type.Proxy (Proxy (..))


localIsos :: forall m m'
           . MonadEffect m
          => Monad m'
          => SpecT m Unit m' Unit
localIsos =
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
