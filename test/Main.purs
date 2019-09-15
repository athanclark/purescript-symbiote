module Test.Main where

import Test.Serialization.Symbiote
  (class SymbioteOperation, class Symbiote, Topic (..), register, SymbioteT, EitherOp, simpleTest)
import Test.Serialization.Symbiote.Argonaut (ToArgonaut)

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.NonEmpty (NonEmpty (..))
import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Test.Spec (describe, pending, it)
import Test.Spec.Runner (runSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Type.Proxy (Proxy (..))


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "All Tests" do
    simpleTests
  where
    simpleTests = describe "Simple Tests" do
      it "Unit over id" (simpleTest unitSuite)
      it "Int over various" (simpleTest intSuite)
      where
        unitSuite :: SymbioteT (EitherOp Unit' Unit'Operation) Aff Unit
        unitSuite = register (Topic "Unit") 100
          (Proxy :: Proxy {value :: Unit', operation :: Unit'Operation})
        intSuite :: SymbioteT (EitherOp Int' Int'Operation) Aff Unit
        intSuite = register (Topic "Int") 100
          (Proxy :: Proxy {value :: Int', operation :: Int'Operation})



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
instance symbioteOperationUnit' :: SymbioteOperation Unit' Unit'Operation where
  perform IdUnit' x = x


newtype Int' = Int' Int
derive instance genericInt' :: Generic Int' _
derive newtype instance showInt' :: Show Int'
derive newtype instance eqInt' :: Eq Int'
derive newtype instance arbitraryInt' :: Arbitrary Int'
derive newtype instance encodeJsonInt' :: EncodeJson Int'
derive newtype instance decodeJsonInt' :: DecodeJson Int'
data Int'Operation
  = AddInt Int
  | DelInt Int
  | DivInt Int
  | MulInt Int
  | ModInt Int
derive instance genericInt'Operation :: Generic Int'Operation _
instance showInt'Operation :: Show Int'Operation where
  show = genericShow
instance eqInt'Operation :: Eq Int'Operation where
  eq = genericEq
instance arbitraryInt'Operation :: Arbitrary Int'Operation where
  arbitrary = oneOf $ NonEmpty (AddInt <$> arbitrary)
    [ DelInt <$> arbitrary
    , DivInt <$> arbitrary
    , MulInt <$> arbitrary
    , ModInt <$> arbitrary
    ]
instance encodeJsonInt'Operation :: EncodeJson Int'Operation where
  encodeJson x = case x of
    AddInt y -> encodeJson {add: y}
    DelInt y -> encodeJson {del: y}
    DivInt y -> encodeJson {div: y}
    MulInt y -> encodeJson {mul: y}
    ModInt y -> encodeJson {mod: y}
instance decodeJsonInt'Operation :: DecodeJson Int'Operation where
  decodeJson x = tryAdd <|> tryDel <|> tryDiv <|> tryMul <|> tryMod
    where
      tryAdd = do
        ({add} :: {add :: Int}) <- decodeJson x
        pure (AddInt add)
      tryDel = do
        ({del} :: {del :: Int}) <- decodeJson x
        pure (DelInt del)
      tryDiv = do
        ({div} :: {div :: Int}) <- decodeJson x
        pure (DivInt div)
      tryMul = do
        ({mul} :: {mul :: Int}) <- decodeJson x
        pure (MulInt mul)
      tryMod = do
        ({mod} :: {mod :: Int}) <- decodeJson x
        pure (ModInt mod)
instance symbioteOperationInt' :: SymbioteOperation Int' Int'Operation where
  perform op (Int' x) = case op of
    AddInt y -> Int' (x + y)
    DelInt y -> Int' (x - y)
    DivInt y -> Int' (x `div` y)
    MulInt y -> Int' (x * y)
    ModInt y -> Int' (x `mod` y)
