module Test.Main.Sanity where

import Test.Main.Types
  ( Unit', Unit'Operation
  , Int', Int'Operation
  , Number', Number'Operation
  , Array', Array'Operation
  , Json', Json'Operation)
import Test.Serialization.Symbiote (simpleTest, SymbioteT, SimpleSerialization, Topic (..), register)
import Test.Serialization.Symbiote.ArrayBuffer (ToArrayBuffer)
import Test.Serialization.Symbiote.Argonaut (ToArgonaut, ShowJson)
import Test.Spec (describe, it, SpecT)

import Prelude
import Data.UInt (UInt)
import Data.Either (Either)
import Data.ArrayBuffer.Typed.Unsafe (AV)
import Data.ArrayBuffer.Types (Uint8)
import Effect.Aff (Aff)
import Type.Proxy (Proxy (..))


sanityTests :: forall m'. Monad m' => SpecT Aff Unit m' Unit
sanityTests =
  describe "Symbiote Sanity Checks" do
    simpleTests
    arraybufferTests
    jsonTests
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
