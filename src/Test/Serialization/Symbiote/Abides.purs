-- | This module provides newtypes for ensuring consistent functionality with respect to various class laws:
-- | Monoids, SemiRing, etc are all included via the
-- | <https://hackage.haskell.org/package/abides abides> library. Note: This only verifies the /consistency/
-- | of behavior between platforms - if both platforms are broken (return @False@) /consistently/, the tests
-- | will pass. Prevent this by implementing a local test suite with
-- | <https://hackage.haskell.org/package/QuickCheck QuickCheck>, and use the abides property tests
-- | directly.

module Test.Serialization.Symbiote.Abides where

import Prelude
import Data.Argonaut
  ( class EncodeJson, class DecodeJson, encodeJson, decodeJson, (~>), (:=), jsonEmptyObject, (.:))
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer, putArrayBuffer, readArrayBuffer)
import Data.ArrayBuffer.Class.Types (Uint8 (..))
import Data.UInt (fromInt, toInt)
import Data.Either (Either (Left))
import Data.NonEmpty (NonEmpty (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Enum (class BoundedEnum, class Enum)
import Control.Alternative ((<|>))
import Test.Abides.Data.Semigroup as Semigroup
import Test.Abides.Data.Monoid as Monoid
import Test.Abides.Data.Eq as Eq
import Test.Abides.Data.Ord as Ord
import Test.Abides.Data.BoundedEnum as Enum
import Test.Abides.Data.Semiring as Semiring
import Test.Abides.Data.Ring as Ring
import Test.Abides.Data.CommutativeRing as CommutativeRing
import Test.Abides.Data.DivisionRing as DivisionRing
import Test.Abides.Data.EuclideanRing as EuclideanRing
import Test.Serialization.Symbiote.Core (class SymbioteOperation, perform)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)


newtype AbidesSemigroup a = AbidesSemigroup a
derive instance genericAbidesSemigroup :: Generic a a' => Generic (AbidesSemigroup a) _
derive newtype instance eqAbidesSemigroup :: Eq a => Eq (AbidesSemigroup a)
derive newtype instance showAbidesSemigroup :: Show a => Show (AbidesSemigroup a)
derive newtype instance semigroupAbidesSemigroup :: Semigroup a => Semigroup (AbidesSemigroup a)
derive newtype instance arbitraryAbidesSemigroup :: Arbitrary a => Arbitrary (AbidesSemigroup a)
derive newtype instance encodeJsonAbidesSemigroup :: EncodeJson a => EncodeJson (AbidesSemigroup a)
derive newtype instance decodeJsonAbidesSemigroup :: DecodeJson a => DecodeJson (AbidesSemigroup a)
derive newtype instance encodeArrayBufferAbidesSemigroup :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesSemigroup a)
derive newtype instance decodeArrayBufferAbidesSemigroup :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesSemigroup a)

newtype AbidesMonoid a = AbidesMonoid a
derive instance genericAbidesMonoid :: Generic a a' => Generic (AbidesMonoid a) _
derive newtype instance eqAbidesMonoid :: Eq a => Eq (AbidesMonoid a)
derive newtype instance showAbidesMonoid :: Show a => Show (AbidesMonoid a)
derive newtype instance semigroupAbidesMonoid :: Semigroup a => Semigroup (AbidesMonoid a)
derive newtype instance monoidAbidesMonoid :: Monoid a => Monoid (AbidesMonoid a)
derive newtype instance arbitraryAbidesMonoid :: Arbitrary a => Arbitrary (AbidesMonoid a)
derive newtype instance encodeJsonAbidesMonoid :: EncodeJson a => EncodeJson (AbidesMonoid a)
derive newtype instance decodeJsonAbidesMonoid :: DecodeJson a => DecodeJson (AbidesMonoid a)
derive newtype instance encodeArrayBufferAbidesMonoid :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesMonoid a)
derive newtype instance decodeArrayBufferAbidesMonoid :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesMonoid a)

newtype AbidesEq a = AbidesEq a
derive instance genericAbidesEq :: Generic a a' => Generic (AbidesEq a) _
derive newtype instance eqAbidesEq :: Eq a => Eq (AbidesEq a)
derive newtype instance showAbidesEq :: Show a => Show (AbidesEq a)
derive newtype instance arbitraryAbidesEq :: Arbitrary a => Arbitrary (AbidesEq a)
derive newtype instance encodeJsonAbidesEq :: EncodeJson a => EncodeJson (AbidesEq a)
derive newtype instance decodeJsonAbidesEq :: DecodeJson a => DecodeJson (AbidesEq a)
derive newtype instance encodeArrayBufferAbidesEq :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesEq a)
derive newtype instance decodeArrayBufferAbidesEq :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesEq a)

newtype AbidesOrd a = AbidesOrd a
derive instance genericAbidesOrd :: Generic a a' => Generic (AbidesOrd a) _
derive newtype instance eqAbidesOrd :: Eq a => Eq (AbidesOrd a)
derive newtype instance ordAbidesOrd :: Ord a => Ord (AbidesOrd a)
derive newtype instance showAbidesOrd :: Show a => Show (AbidesOrd a)
derive newtype instance arbitraryAbidesOrd :: Arbitrary a => Arbitrary (AbidesOrd a)
derive newtype instance encodeJsonAbidesOrd :: EncodeJson a => EncodeJson (AbidesOrd a)
derive newtype instance decodeJsonAbidesOrd :: DecodeJson a => DecodeJson (AbidesOrd a)
derive newtype instance encodeArrayBufferAbidesOrd :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesOrd a)
derive newtype instance decodeArrayBufferAbidesOrd :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesOrd a)

newtype AbidesEnum a = AbidesEnum a
derive instance genericAbidesEnum :: Generic a a' => Generic (AbidesEnum a) _
derive newtype instance eqAbidesEnum :: Eq a => Eq (AbidesEnum a)
derive newtype instance ordAbidesEnum :: Ord a => Ord (AbidesEnum a)
derive newtype instance boundedAbidesEnum :: Bounded a => Bounded (AbidesEnum a)
derive newtype instance enumAbidesEnum :: Enum a => Enum (AbidesEnum a)
derive newtype instance boundedEnumAbidesEnum :: BoundedEnum a => BoundedEnum (AbidesEnum a)
derive newtype instance showAbidesEnum :: Show a => Show (AbidesEnum a)
derive newtype instance arbitraryAbidesEnum :: Arbitrary a => Arbitrary (AbidesEnum a)
derive newtype instance encodeJsonAbidesEnum :: EncodeJson a => EncodeJson (AbidesEnum a)
derive newtype instance decodeJsonAbidesEnum :: DecodeJson a => DecodeJson (AbidesEnum a)
derive newtype instance encodeArrayBufferAbidesEnum :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesEnum a)
derive newtype instance decodeArrayBufferAbidesEnum :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesEnum a)

newtype AbidesSemiring a = AbidesSemiring a
derive instance genericAbidesSemiring :: Generic a a' => Generic (AbidesSemiring a) _
derive newtype instance eqAbidesSemiring :: Eq a => Eq (AbidesSemiring a)
derive newtype instance showAbidesSemiring :: Show a => Show (AbidesSemiring a)
derive newtype instance semiringAbidesSemiring :: Semiring a => Semiring (AbidesSemiring a)
derive newtype instance arbitraryAbidesSemiring :: Arbitrary a => Arbitrary (AbidesSemiring a)
derive newtype instance encodeJsonAbidesSemiring :: EncodeJson a => EncodeJson (AbidesSemiring a)
derive newtype instance decodeJsonAbidesSemiring :: DecodeJson a => DecodeJson (AbidesSemiring a)
derive newtype instance encodeArrayBufferAbidesSemiring :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesSemiring a)
derive newtype instance decodeArrayBufferAbidesSemiring :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesSemiring a)

newtype AbidesRing a = AbidesRing a
derive instance genericAbidesRing :: Generic a a' => Generic (AbidesRing a) _
derive newtype instance eqAbidesRing :: Eq a => Eq (AbidesRing a)
derive newtype instance showAbidesRing :: Show a => Show (AbidesRing a)
derive newtype instance semiringAbidesRing :: Semiring a => Semiring (AbidesRing a)
derive newtype instance ringAbidesRing :: Ring a => Ring (AbidesRing a)
derive newtype instance arbitraryAbidesRing :: Arbitrary a => Arbitrary (AbidesRing a)
derive newtype instance encodeJsonAbidesRing :: EncodeJson a => EncodeJson (AbidesRing a)
derive newtype instance decodeJsonAbidesRing :: DecodeJson a => DecodeJson (AbidesRing a)
derive newtype instance encodeArrayBufferAbidesRing :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesRing a)
derive newtype instance decodeArrayBufferAbidesRing :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesRing a)

newtype AbidesCommutativeRing a = AbidesCommutativeRing a
derive instance genericAbidesCommutativeRing :: Generic a a' => Generic (AbidesCommutativeRing a) _
derive newtype instance eqAbidesCommutativeRing :: Eq a => Eq (AbidesCommutativeRing a)
derive newtype instance showAbidesCommutativeRing :: Show a => Show (AbidesCommutativeRing a)
derive newtype instance semiringAbidesCommutativeRing :: Semiring a => Semiring (AbidesCommutativeRing a)
derive newtype instance ringAbidesCommutativeRing :: Ring a => Ring (AbidesCommutativeRing a)
derive newtype instance commutativeRingAbidesCommutativeRing :: CommutativeRing a => CommutativeRing (AbidesCommutativeRing a)
derive newtype instance arbitraryAbidesCommutativeRing :: Arbitrary a => Arbitrary (AbidesCommutativeRing a)
derive newtype instance encodeJsonAbidesCommutativeRing :: EncodeJson a => EncodeJson (AbidesCommutativeRing a)
derive newtype instance decodeJsonAbidesCommutativeRing :: DecodeJson a => DecodeJson (AbidesCommutativeRing a)
derive newtype instance encodeArrayBufferAbidesCommutativeRing :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesCommutativeRing a)
derive newtype instance decodeArrayBufferAbidesCommutativeRing :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesCommutativeRing a)

newtype AbidesDivisionRing a = AbidesDivisionRing a
derive instance genericAbidesDivisionRing :: Generic a a' => Generic (AbidesDivisionRing a) _
derive newtype instance eqAbidesDivisionRing :: Eq a => Eq (AbidesDivisionRing a)
derive newtype instance showAbidesDivisionRing :: Show a => Show (AbidesDivisionRing a)
derive newtype instance semiringAbidesDivisionRing :: Semiring a => Semiring (AbidesDivisionRing a)
derive newtype instance ringAbidesDivisionRing :: Ring a => Ring (AbidesDivisionRing a)
derive newtype instance divisionRingAbidesDivisionRing :: DivisionRing a => DivisionRing (AbidesDivisionRing a)
derive newtype instance arbitraryAbidesDivisionRing :: Arbitrary a => Arbitrary (AbidesDivisionRing a)
derive newtype instance encodeJsonAbidesDivisionRing :: EncodeJson a => EncodeJson (AbidesDivisionRing a)
derive newtype instance decodeJsonAbidesDivisionRing :: DecodeJson a => DecodeJson (AbidesDivisionRing a)
derive newtype instance encodeArrayBufferAbidesDivisionRing :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesDivisionRing a)
derive newtype instance decodeArrayBufferAbidesDivisionRing :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesDivisionRing a)

newtype AbidesEuclideanRing a = AbidesEuclideanRing a
derive instance genericAbidesEuclideanRing :: Generic a a' => Generic (AbidesEuclideanRing a) _
derive newtype instance eqAbidesEuclideanRing :: Eq a => Eq (AbidesEuclideanRing a)
derive newtype instance showAbidesEuclideanRing :: Show a => Show (AbidesEuclideanRing a)
derive newtype instance semiringAbidesEuclideanRing :: Semiring a => Semiring (AbidesEuclideanRing a)
derive newtype instance ringAbidesEuclideanRing :: Ring a => Ring (AbidesEuclideanRing a)
derive newtype instance commutativeRingAbidesEuclideanRing :: CommutativeRing a => CommutativeRing (AbidesEuclideanRing a)
derive newtype instance euclideanRingAbidesEuclideanRing :: EuclideanRing a => EuclideanRing (AbidesEuclideanRing a)
derive newtype instance arbitraryAbidesEuclideanRing :: Arbitrary a => Arbitrary (AbidesEuclideanRing a)
derive newtype instance encodeJsonAbidesEuclideanRing :: EncodeJson a => EncodeJson (AbidesEuclideanRing a)
derive newtype instance decodeJsonAbidesEuclideanRing :: DecodeJson a => DecodeJson (AbidesEuclideanRing a)
derive newtype instance encodeArrayBufferAbidesEuclideanRing :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesEuclideanRing a)
derive newtype instance decodeArrayBufferAbidesEuclideanRing :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesEuclideanRing a)

newtype AbidesField a = AbidesField a
derive instance genericAbidesField :: Generic a a' => Generic (AbidesField a) _
derive newtype instance eqAbidesField :: Eq a => Eq (AbidesField a)
derive newtype instance showAbidesField :: Show a => Show (AbidesField a)
derive newtype instance semiringAbidesField :: Semiring a => Semiring (AbidesField a)
derive newtype instance ringAbidesField :: Ring a => Ring (AbidesField a)
derive newtype instance commutativeRingAbidesField :: CommutativeRing a => CommutativeRing (AbidesField a)
derive newtype instance euclideanRingAbidesField :: EuclideanRing a => EuclideanRing (AbidesField a)
derive newtype instance divisionRingAbidesField :: DivisionRing a => DivisionRing (AbidesField a)
derive newtype instance arbitraryAbidesField :: Arbitrary a => Arbitrary (AbidesField a)
derive newtype instance encodeJsonAbidesField :: EncodeJson a => EncodeJson (AbidesField a)
derive newtype instance decodeJsonAbidesField :: DecodeJson a => DecodeJson (AbidesField a)
derive newtype instance encodeArrayBufferAbidesField :: EncodeArrayBuffer a => EncodeArrayBuffer (AbidesField a)
derive newtype instance decodeArrayBufferAbidesField :: DecodeArrayBuffer a => DecodeArrayBuffer (AbidesField a)




instance symbioteOperationAbidesSemigroup ::
  ( Semigroup a, Eq a
  ) => SymbioteOperation (AbidesSemigroup a) Boolean (AbidesSemigroupOperation a) where
  perform op x = case op of
    SemigroupAssociative y z -> Semigroup.associative x y z
data AbidesSemigroupOperation a
  = SemigroupAssociative (AbidesSemigroup a) (AbidesSemigroup a)
derive instance genericAbidesSemigroupOperation :: Generic a a' => Generic (AbidesSemigroupOperation a) _
instance showAbidesSemigroupOperation :: (Show a, Generic a a') => Show (AbidesSemigroupOperation a) where
  show = genericShow
instance arbitraryAbidesSemigroupOperation :: Arbitrary a => Arbitrary (AbidesSemigroupOperation a) where
  arbitrary = SemigroupAssociative <$> arbitrary <*> arbitrary
instance encodeJsonAbidesSemigroupOperation :: EncodeJson a => EncodeJson (AbidesSemigroupOperation a) where
  encodeJson op = case op of
    SemigroupAssociative y z -> "associative" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
instance decodeJsonAbidesSemigroupOperation :: DecodeJson a => DecodeJson (AbidesSemigroupOperation a) where
  decodeJson json = do
    o <- decodeJson json
    o' <- o .: "associative"
    SemigroupAssociative <$> o' .: "y" <*> o' .: "z"
-- instance Serialize a => Serialize (AbidesSemigroupOperation a) where
--   put op = case op of
--     SemigroupAssociative y z -> put y *> put z
--   get = SemigroupAssociative <$> get <*> get

instance symbioteOperationsAbideMonoid ::
  (Monoid a, Eq a) => SymbioteOperation (AbidesMonoid a) Boolean (AbidesMonoidOperation a) where
  perform op x@(AbidesMonoid x') = case op of
    MonoidSemigroup op' -> perform op' (AbidesSemigroup x')
    MonoidLeftIdentity -> Monoid.leftIdentity x
    MonoidRightIdentity -> Monoid.rightIdentity x
data AbidesMonoidOperation a
  = MonoidSemigroup (AbidesSemigroupOperation a)
  | MonoidLeftIdentity
  | MonoidRightIdentity
derive instance genericAbidesMonoidOperation :: Generic a a' => Generic (AbidesMonoidOperation a) _
instance showAbidesMonoidOperation :: (Show a, Generic a a') => Show (AbidesMonoidOperation a) where
  show = genericShow
instance arbitraryMonoidOperation :: Arbitrary a => Arbitrary (AbidesMonoidOperation a) where
  arbitrary = oneOf $ NonEmpty
      (MonoidSemigroup <$> arbitrary)
    [ pure MonoidLeftIdentity
    , pure MonoidRightIdentity
    ]
instance encodeJsonAbidesMonoidOperation :: EncodeJson a => EncodeJson (AbidesMonoidOperation a) where
  encodeJson op = case op of
    MonoidSemigroup op' -> "semigroup" := op' ~> jsonEmptyObject
    MonoidLeftIdentity -> encodeJson "leftIdentity"
    MonoidRightIdentity -> encodeJson "rightIdentity"
instance decodeJsonAbidesMonoidOperation :: DecodeJson a => DecodeJson (AbidesMonoidOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        MonoidSemigroup <$> o .: "semigroup"
      string = do
        s <- decodeJson json
        case s of
          _ | s == "leftIdentity" -> pure MonoidLeftIdentity
            | s == "rightIdentity" -> pure MonoidRightIdentity
            | otherwise -> Left "AbidesMonoidOperation a"
-- instance Serialize a => Serialize (AbidesMonoidOperation a) where
--   put op = case op of
--     MonoidSemigroup op' -> putWord8 0 *> put op'
--     MonoidLeftIdentity -> putWord8 1
--     MonoidRightIdentity -> putWord8 2
--   get = do
--     x <- getWord8
--     case x of
--       0 -> MonoidSemigroup <$> get
--       1 -> pure MonoidLeftIdentity
--       2 -> pure MonoidRightIdentity
--       _ -> fail "AbidesMonoidOperation a"

instance symbioteOperationAbidesEq :: (Eq a) => SymbioteOperation (AbidesEq a) Boolean (AbidesEqOperation a) where
  perform op x = case op of
    EqReflexive -> Eq.reflexive x
    EqSymmetry y -> Eq.symmetry x y
    EqTransitive y z -> Eq.transitive x y z
    EqNegation y -> Eq.negation x y
data AbidesEqOperation a
  = EqReflexive
  | EqSymmetry (AbidesEq a)
  | EqTransitive (AbidesEq a) (AbidesEq a)
  | EqNegation (AbidesEq a)
derive instance genericAbidesEqOperation :: Generic a a' => Generic (AbidesEqOperation a) _
instance showAbidesEqOperation :: (Show a, Generic a a') => Show (AbidesEqOperation a) where
  show = genericShow
instance arbitraryAbidesEqOperation :: Arbitrary a => Arbitrary (AbidesEqOperation a) where
  arbitrary = oneOf $ NonEmpty
      (EqSymmetry <$> arbitrary)
    [ pure EqReflexive
    , EqTransitive <$> arbitrary <*> arbitrary
    , EqNegation <$> arbitrary
    ]
instance encodeJsonAbidesEqOperation :: EncodeJson a => EncodeJson (AbidesEqOperation a) where
  encodeJson op = case op of
    EqSymmetry y -> "symmetry" := y ~> jsonEmptyObject
    EqReflexive -> encodeJson "reflexive"
    EqTransitive y z -> "transitive" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
    EqNegation y -> "negation" := y ~> jsonEmptyObject
instance decodeJsonAbidesEqOperation :: DecodeJson a => DecodeJson (AbidesEqOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        let transitive = do
              o' <- o .: "transitive"
              EqTransitive <$> o' .: "y" <*> o' .: "z"
            symmetry = EqSymmetry <$> o .: "symmetry"
            negation = EqNegation <$> o .: "negation"
        transitive <|> symmetry <|> negation
      string = do
        s <- decodeJson json
        case s of
          _ | s == "reflexive" -> pure EqReflexive
            | otherwise -> Left "AbidesEqOperation a"
-- instance Serialize a => Serialize (AbidesEqOperation a) where
--   put op = case op of
--     EqSymmetry y -> putWord8 0 *> put y
--     EqReflexive -> putWord8 1
--     EqTransitive y z -> putWord8 2 *> put y *> put z
--     EqNegation y -> putWord8 3 *> put y
--   get = do
--     x <- getWord8
--     case x of
--       0 -> EqSymmetry <$> get
--       1 -> pure EqReflexive
--       2 -> EqTransitive <$> get <*> get
--       3 -> EqNegation <$> get
--       _ -> fail "Operation (AbidesEq a)"

instance symbioteOperationAbidesOrd :: (Ord a) => SymbioteOperation (AbidesOrd a) Boolean (AbidesOrdOperation a) where
  perform op x = case op of
    OrdReflexive -> Ord.reflexive x
    OrdAntiSymmetry y -> Ord.antisymmetry x y
    OrdTransitive y z -> Ord.transitive x y z
data AbidesOrdOperation a
  = OrdReflexive
  | OrdAntiSymmetry (AbidesOrd a)
  | OrdTransitive (AbidesOrd a) (AbidesOrd a)
derive instance genericAbidesOrdOperation :: Generic a a' => Generic (AbidesOrdOperation a) _
instance showAbidesOrdOperation :: (Show a, Generic a a') => Show (AbidesOrdOperation a) where
  show = genericShow
instance arbitraryAbidesOrdOperation :: Arbitrary a => Arbitrary (AbidesOrdOperation a) where
  arbitrary = oneOf $ NonEmpty
      (OrdAntiSymmetry <$> arbitrary)
    [ pure OrdReflexive
    , OrdTransitive <$> arbitrary <*> arbitrary
    ]
instance encodeJsonAbidesOrdOperation :: EncodeJson a => EncodeJson (AbidesOrdOperation a) where
  encodeJson op = case op of
    OrdReflexive -> encodeJson "reflexive"
    OrdAntiSymmetry y -> "antisymmetry" := y ~> jsonEmptyObject
    OrdTransitive y z -> "transitive" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
instance decodeJsonAbidesOrdOperation :: DecodeJson a => DecodeJson (AbidesOrdOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        let transitive = do
              o' <- o .: "transitive"
              OrdTransitive <$> o' .: "y" <*> o' .: "z"
            antisymmetry = OrdAntiSymmetry <$> o .: "antisymmetry"
        transitive <|> antisymmetry
      string = do
        s <- decodeJson json
        case s of
          _ | s == "reflexive" -> pure OrdReflexive
            | otherwise -> Left "AbidesOrdOperation a"
-- instance Serialize a => Serialize (AbidesOrdOperation a) where
--   put op = case op of
--     OrdReflexive -> putWord8 0
--     OrdAntiSymmetry y -> putWord8 1 *> put y
--     OrdTransitive y z -> putWord8 2 *> put y *> put z
--   get = do
--     x <- getWord8
--     case x of
--       0 -> pure OrdReflexive
--       1 -> OrdAntiSymmetry <$> get
--       2 -> OrdTransitive <$> get <*> get
--       _ -> fail "Operation (AbidesOrd a)"

instance symbioteOperationAbidesEnum :: (BoundedEnum a, Ord a) => SymbioteOperation (AbidesEnum a) Boolean (AbidesEnumOperation a) where
  perform op x = case op of
    EnumCompareHom y -> Enum.compareHom x y
    EnumPredSucc -> Enum.predsucc x
    EnumSuccPred -> Enum.succpred x
data AbidesEnumOperation a
  = EnumCompareHom (AbidesEnum a)
  | EnumPredSucc
  | EnumSuccPred
derive instance genericAbidesEnumOperation :: Generic a a' => Generic (AbidesEnumOperation a) _
instance showAbidesEnumOperation :: (Show a, Generic a a') => Show (AbidesEnumOperation a) where
  show = genericShow
instance arbitraryAbidesEnumOperation :: Arbitrary a => Arbitrary (AbidesEnumOperation a) where
  arbitrary = oneOf $ NonEmpty
      (EnumCompareHom <$> arbitrary)
    [ pure EnumPredSucc
    , pure EnumSuccPred
    ]
instance encodeJsonAbidesEnumOperation :: EncodeJson a => EncodeJson (AbidesEnumOperation a) where
  encodeJson op = case op of
    EnumCompareHom y -> "compareHom" := y ~> jsonEmptyObject
    EnumPredSucc -> encodeJson "predsucc"
    EnumSuccPred -> encodeJson "succpred"
instance decodeJsonAbidesEnumOperation :: DecodeJson a => DecodeJson (AbidesEnumOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        EnumCompareHom <$> o .: "compareHom"
      string = do
        s <- decodeJson json
        case s of
          _ | s == "predsucc" -> pure EnumPredSucc
            | s == "succpred" -> pure EnumSuccPred
            | otherwise -> Left "AbidesEnumOperation a"
-- instance Serialize a => Serialize (AbidesEnumOperation a) where
--   put op = case op of
--     EnumCompareHom y -> putWord8 0 *> put y
--     EnumPredSucc -> putWord8 1
--     EnumSuccPred -> putWord8 2
--   get = do
--     x <- getWord8
--     case x of
--       0 -> EnumCompareHom <$> get
--       1 -> pure EnumPredSucc
--       2 -> pure EnumSuccPred
--       _ -> fail "Operation (AbidesEnum a)"

instance symbioteOperationAbidesSemiring :: (Semiring a, Eq a) => SymbioteOperation (AbidesSemiring a) Boolean (AbidesSemiringOperation a) where
  perform op x = case op of
    SemiringCommutativeMonoid y z -> Semiring.commutativeMonoid x y z
    SemiringMonoid y z -> Semiring.monoid x y z
    SemiringLeftDistributive y z -> Semiring.leftDistributive x y z
    SemiringRightDistributive y z -> Semiring.rightDistributive x y z
    SemiringAnnihilation -> Semiring.annihilation x
data AbidesSemiringOperation a
  = SemiringCommutativeMonoid (AbidesSemiring a) (AbidesSemiring a)
  | SemiringMonoid (AbidesSemiring a) (AbidesSemiring a)
  | SemiringLeftDistributive (AbidesSemiring a) (AbidesSemiring a)
  | SemiringRightDistributive (AbidesSemiring a) (AbidesSemiring a)
  | SemiringAnnihilation
derive instance genericAbidesSemiringOperation :: Generic a a' => Generic (AbidesSemiringOperation a) _
instance showAbidesSemiringOperation :: (Show a, Generic a a') => Show (AbidesSemiringOperation a) where
  show = genericShow
instance arbitraryAbidesSemiringOperation :: Arbitrary a => Arbitrary (AbidesSemiringOperation a) where
  arbitrary = oneOf $ NonEmpty
      (SemiringCommutativeMonoid <$> arbitrary <*> arbitrary)
    [ SemiringMonoid <$> arbitrary <*> arbitrary
    , SemiringLeftDistributive <$> arbitrary <*> arbitrary
    , SemiringRightDistributive <$> arbitrary <*> arbitrary
    , pure SemiringAnnihilation
    ]
instance encodeJsonAbidesSemiringOperation :: EncodeJson a => EncodeJson (AbidesSemiringOperation a) where
  encodeJson op = case op of
    SemiringCommutativeMonoid y z -> "commutativeMonoid" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
    SemiringMonoid y z -> "monoid" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
    SemiringLeftDistributive y z -> "leftDistributive" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
    SemiringRightDistributive y z -> "rightDistributive" := ("y" := y ~> "z" := z ~> jsonEmptyObject) ~> jsonEmptyObject
    SemiringAnnihilation -> encodeJson "annihilation"
instance decodeJsonAbidesSemiringOperation :: DecodeJson a => DecodeJson (AbidesSemiringOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        let commutativeMonoid = do
              o' <- o .: "commutativeMonoid"
              SemiringCommutativeMonoid <$> o' .: "y" <*> o' .: "z"
            monoid = do
              o' <- o .: "monoid"
              SemiringMonoid <$> o' .: "y" <*> o' .: "z"
            leftDistributive = do
              o' <- o .: "leftDistributive"
              SemiringLeftDistributive <$> o' .: "y" <*> o' .: "z"
            rightDistributive = do
              o' <- o .: "rightDistributive"
              SemiringRightDistributive <$> o' .: "y" <*> o' .: "z"
        commutativeMonoid <|> monoid <|> leftDistributive <|> rightDistributive
      string = do
        s <- decodeJson json
        case s of
          _ | s == "annihilation" -> pure SemiringAnnihilation
            | otherwise -> Left "AbidesSemiringOperation a"
-- instance Serialize a => Serialize (AbidesSemiringOperation a) where
--   put op = case op of
--     SemiringCommutativeMonoid y z -> putWord8 0 *> put y *> put z
--     SemiringMonoid y z -> putWord8 1 *> put y *> put z
--     SemiringLeftDistributive y z -> putWord8 2 *> put y *> put z
--     SemiringRightDistributive y z -> putWord8 3 *> put y *> put z
--     SemiringAnnihilation -> putWord8 4
--   get = do
--     x <- getWord8
--     case x of
--       0 -> SemiringCommutativeMonoid <$> get <*> get
--       1 -> SemiringMonoid <$> get <*> get
--       2 -> SemiringLeftDistributive <$> get <*> get
--       3 -> SemiringRightDistributive <$> get <*> get
--       4 -> pure SemiringAnnihilation
--       _ -> fail "Operation (AbidesSemiring a)"

instance symbioteOperationAbidesRing :: (Ring a, Eq a) => SymbioteOperation (AbidesRing a) Boolean (AbidesRingOperation a) where
  perform op x@(AbidesRing x') = case op of
    RingSemiring op' -> perform op' (AbidesSemiring x')
    RingAdditiveInverse -> Ring.additiveInverse x
data AbidesRingOperation a
  = RingSemiring (AbidesSemiringOperation a)
  | RingAdditiveInverse
derive instance genericAbidesRingOperation :: Generic a a' => Generic (AbidesRingOperation a) _
instance showAbidesRingOperation :: (Show a, Generic a a') => Show (AbidesRingOperation a) where
  show = genericShow
instance arbitraryAbidesRingOperation :: Arbitrary a => Arbitrary (AbidesRingOperation a) where
  arbitrary = oneOf $ NonEmpty
      (RingSemiring <$> arbitrary)
    [ pure RingAdditiveInverse
    ]
instance encodeJsonAbidesRingOperation :: EncodeJson a => EncodeJson (AbidesRingOperation a) where
  encodeJson op = case op of
    RingSemiring op' -> "semiring" := op' ~> jsonEmptyObject
    RingAdditiveInverse -> encodeJson "additiveInverse"
instance decodeJsonAbidesRingOperation :: DecodeJson a => DecodeJson (AbidesRingOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        RingSemiring <$> o .: "semiring"
      string = do
        s <- decodeJson json
        case s of
          _ | s == "additiveInverse" -> pure RingAdditiveInverse
            | otherwise -> Left "AbidesRingOperation a"
-- instance Serialize a => Serialize (AbidesRingOperation a) where
--   put op = case op of
--     RingSemiring op' -> putWord8 0 *> put op'
--     RingAdditiveInverse -> putWord8 1
--   get = do
--     x <- getWord8
--     case x of
--       0 -> RingSemiring <$> get
--       1 -> pure RingAdditiveInverse
--       _ -> fail "AbidesRingOperation a"

instance symbioteOperationCommutativeRing :: (CommutativeRing a, Eq a) => SymbioteOperation (AbidesCommutativeRing a) Boolean (AbidesCommutativeRingOperation a) where
  perform op x@(AbidesCommutativeRing x') = case op of
    CommutativeRingRing op' -> perform op' (AbidesRing x')
    CommutativeRingCommutative y -> CommutativeRing.commutative x y
data AbidesCommutativeRingOperation a
  = CommutativeRingRing (AbidesRingOperation a)
  | CommutativeRingCommutative (AbidesCommutativeRing a)
derive instance genericAbidesCommutativeRingOperation :: Generic a a' => Generic (AbidesCommutativeRingOperation a) _
instance showAbidesCommutativeRingOperation :: (Show a, Generic a a') => Show (AbidesCommutativeRingOperation a) where
  show = genericShow
instance arbitraryAbidesCommutativeRingOperation :: Arbitrary a => Arbitrary (AbidesCommutativeRingOperation a) where
  arbitrary = oneOf $ NonEmpty
      (CommutativeRingRing <$> arbitrary)
    [ CommutativeRingCommutative <$> arbitrary
    ]
instance encodeJsonAbidesCommutativeRingOperation :: EncodeJson a => EncodeJson (AbidesCommutativeRingOperation a) where
  encodeJson op = case op of
    CommutativeRingRing op' -> "ring" := op' ~> jsonEmptyObject
    CommutativeRingCommutative y -> "commutative" := y ~> jsonEmptyObject
instance decodeJsonAbidesCommutativeRingOperation :: DecodeJson a => DecodeJson (AbidesCommutativeRingOperation a) where
  decodeJson json = do
    o <- decodeJson json
    let ring = CommutativeRingRing <$> o .: "ring"
        commutative = CommutativeRingCommutative <$> o .: "commutative"
    ring <|> commutative
-- instance Serialize a => Serialize (AbidesCommutativeRingOperation a) where
--   put op = case op of
--     CommutativeRingRing op' -> putWord8 0 *> put op'
--     CommutativeRingCommutative y -> putWord8 1 *> put y
--   get = do
--     x <- getWord8
--     case x of
--       0 -> CommutativeRingRing <$> get
--       1 -> CommutativeRingCommutative <$> get
--       _ -> fail "AbidesCommutativeRingOperation a"

instance symbioteOperationAbidesDivisionRing :: (DivisionRing a, Eq a) => SymbioteOperation (AbidesDivisionRing a) Boolean (AbidesDivisionRingOperation a) where
  perform op x@(AbidesDivisionRing x') = case op of
    DivisionRingRing op' -> perform op' (AbidesRing x')
    DivisionRingInverse -> DivisionRing.inverse x
data AbidesDivisionRingOperation a
  = DivisionRingRing (AbidesRingOperation a)
  | DivisionRingInverse
derive instance genericDivisionRingOperation :: Generic a a' => Generic (AbidesDivisionRingOperation a) _
instance showAbidesDivisionRingOperation :: (Show a, Generic a a') => Show (AbidesDivisionRingOperation a) where
  show = genericShow
instance arbitraryAbidesDivisionRingOperation :: Arbitrary a => Arbitrary (AbidesDivisionRingOperation a) where
  arbitrary = oneOf $ NonEmpty
      (DivisionRingRing <$> arbitrary)
    [ pure DivisionRingInverse
    ]
instance encodeJsonAbidesDivisionRingOperation :: EncodeJson a => EncodeJson (AbidesDivisionRingOperation a) where
  encodeJson op = case op of
    DivisionRingRing op' -> "ring" := op' ~> jsonEmptyObject
    DivisionRingInverse -> encodeJson "inverse"
instance decodeJsonAbidesDivisionRingOperation :: DecodeJson a => DecodeJson (AbidesDivisionRingOperation a) where
  decodeJson json = object <|> string
    where
      object = do
        o <- decodeJson json
        DivisionRingRing <$> o .: "ring"
      string = do
        s <- decodeJson json
        case s of
          _ | s == "inverse" -> pure DivisionRingInverse
            | otherwise -> Left "AbidesDivisionRingOperation a"
-- instance Serialize a => Serialize (AbidesDivisionRingOperation a) where
--   put op = case op of
--     DivisionRingRing op' -> putWord8 0 *> put op'
--     DivisionRingInverse -> putWord8 1
--   get = do
--     x <- getWord8
--     case x of
--       0 -> DivisionRingRing <$> get
--       1 -> pure DivisionRingInverse
--       _ -> fail "AbidesDivisionRingOperation a"

instance symbioteOperationAbidesEuclideanRing :: (EuclideanRing a, Eq a) => SymbioteOperation (AbidesEuclideanRing a) Boolean (AbidesEuclideanRingOperation a) where
  perform op x@(AbidesEuclideanRing x') = case op of
    EuclideanRingCommutativeRing op' -> perform op' (AbidesCommutativeRing x')
    EuclideanRingIntegralDomain y -> EuclideanRing.integralDomain x y
data AbidesEuclideanRingOperation a
  = EuclideanRingCommutativeRing (AbidesCommutativeRingOperation a)
  | EuclideanRingIntegralDomain (AbidesEuclideanRing a)
derive instance genericAbidesEuclideanRingOperation :: Generic a a' => Generic (AbidesEuclideanRingOperation a) _
instance showAbidesEuclideanRingOperation :: (Show a, Generic a a') => Show (AbidesEuclideanRingOperation a) where
  show = genericShow
instance arbitraryAbidesEuclideanRingOperation :: Arbitrary a => Arbitrary (AbidesEuclideanRingOperation a) where
  arbitrary = oneOf $ NonEmpty
      (EuclideanRingCommutativeRing <$> arbitrary)
    [ EuclideanRingIntegralDomain <$> arbitrary
    ]
instance encodeJsonAbidesEuclideanRingOperation :: EncodeJson a => EncodeJson (AbidesEuclideanRingOperation a) where
  encodeJson op = case op of
    EuclideanRingCommutativeRing op' -> "commutativeRing" := op' ~> jsonEmptyObject
    EuclideanRingIntegralDomain y -> "integralDomain" := y ~> jsonEmptyObject
instance decodeJsonAbidesEuclideanRingOperation :: DecodeJson a => DecodeJson (AbidesEuclideanRingOperation a) where
  decodeJson json = do
    o <- decodeJson json
    let commutativeRing = EuclideanRingCommutativeRing <$> o .: "commutativeRing"
        integralDomain = EuclideanRingIntegralDomain <$> o .: "integralDomain"
    commutativeRing <|> integralDomain
-- instance Serialize a => Serialize (AbidesEuclideanRingOperation a) where
--   put op = case op of
--     EuclideanRingCommutativeRing op' -> putWord8 0 *> put op'
--     EuclideanRingIntegralDomain y -> putWord8 1 *> put y
--   get = do
--     x <- getWord8
--     case x of
--       0 -> EuclideanRingCommutativeRing <$> get
--       1 -> EuclideanRingIntegralDomain <$> get
--       _ -> fail "AbidesEuclideanRingOperation a"

instance symbioteOperationAbidesField :: (Field a, Eq a) => SymbioteOperation (AbidesField a) Boolean (AbidesFieldOperation a) where
  perform op (AbidesField x') = case op of
    FieldDivisionRing op' -> perform op' (AbidesDivisionRing x')
    FieldEuclideanRing op' -> perform op' (AbidesEuclideanRing x')
data AbidesFieldOperation a
  = FieldDivisionRing (AbidesDivisionRingOperation a)
  | FieldEuclideanRing (AbidesEuclideanRingOperation a)
derive instance genericAbidesFieldOperation :: Generic a a' => Generic (AbidesFieldOperation a) _
instance showAbidesFieldOperation :: (Show a, Generic a a') => Show (AbidesFieldOperation a) where
  show = genericShow
instance arbitraryAbidesFieldOperation :: Arbitrary a => Arbitrary (AbidesFieldOperation a) where
  arbitrary = oneOf $ NonEmpty
      (FieldDivisionRing <$> arbitrary)
    [ FieldEuclideanRing <$> arbitrary
    ]
instance encodeJsonAbidesFieldOperation :: EncodeJson a => EncodeJson (AbidesFieldOperation a) where
  encodeJson op = case op of
    FieldDivisionRing op' -> "divisionRing" := op' ~> jsonEmptyObject
    FieldEuclideanRing y -> "euclideanRing" := y ~> jsonEmptyObject
instance decodeJsonAbidesFieldOperation :: DecodeJson a => DecodeJson (AbidesFieldOperation a) where
  decodeJson json = do
    o <- decodeJson json
    let divisionRing = FieldDivisionRing <$> o .: "divisionRing"
        euclideanRing = FieldEuclideanRing <$> o .: "euclideanRing"
    divisionRing <|> euclideanRing
-- instance Serialize a => Serialize (AbidesFieldOperation a) where
--   put op = case op of
--     FieldDivisionRing op' -> putWord8 0 *> put op'
--     FieldEuclideanRing y -> putWord8 1 *> put y
--   get = do
--     x <- getWord8
--     case x of
--       0 -> FieldDivisionRing <$> get
--       1 -> FieldEuclideanRing <$> get
--       _ -> fail "AbidesFieldOperation a"
