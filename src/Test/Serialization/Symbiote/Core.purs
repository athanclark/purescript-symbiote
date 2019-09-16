module Test.Serialization.Symbiote.Core where

import Prelude
import Data.Maybe (Maybe)
import Data.Map (Map)
import Data.Map (empty) as Map
import Data.Exists (Exists, runExists)
import Data.Int (toNumber) as Int
import Data.Generic.Rep (class Generic)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (StateT, execStateT)
import Effect.Ref (Ref)
import Effect.Ref (modify, read) as Ref
import Effect.Class (class MonadEffect, liftEffect)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen (evalGen) as QC
import Random.LCG (randomSeed)
import Partial.Unsafe (unsafePartial)


-- | A type-level relation between a type and appropriate, testable operations on that type.
class SymbioteOperation a op | a -> op, op -> a where
  perform :: op -> a -> a

-- | A synonym for `s`, because we can't quantify the type family and have to store it serialized, but trying to denote that it still "belongs" to `a`.
type Operation a s = s

-- | A serialization format for a particular type, and serialized data type.
class SymbioteOperation a op <= Symbiote a op s | a -> op, op -> a where
  encode :: a -> s
  decode :: s -> Maybe a
  encodeOp :: op -> s
  decodeOp :: s -> Maybe op

-- | Unique name of a type, for a suite of tests
newtype Topic = Topic String
derive instance genericTopic :: Generic Topic _
derive newtype instance eqTopic :: Eq Topic
derive newtype instance ordTopic :: Ord Topic
derive newtype instance showTopic :: Show Topic

-- | Protocol state for a particular topic
data SymbioteProtocol a s
  = MeGenerated
      { value :: a
      , operation :: Operation a s
      , received :: s
      }
  | ThemGenerating
      { gen :: Maybe {value :: s, operation :: s}
      }
  | NotStarted
  | Finished

-- | Protocol generation state
newtype SymbioteGeneration a s = SymbioteGeneration
  { size :: Int
  , protocol :: SymbioteProtocol a s
  }

newGeneration :: forall a s. SymbioteGeneration a s
newGeneration = SymbioteGeneration
  { size: 1
  , protocol: NotStarted
  }

-- | Internal existential state of a registered topic with type's facilities
newtype SymbioteState s a = SymbioteState
  { generate   :: Gen a
  , generateOp :: Gen (Operation a s)
  , equal      :: a -> a -> Boolean
  , maxSize    :: Int
  , generation :: Ref (SymbioteGeneration a s)
  , encode'    :: a -> s
  , decode'    :: s -> Maybe a
  , perform'   :: Operation a s -> a -> a
  }

type SymbioteT s m = ReaderT Boolean (StateT (Map Topic (Exists (SymbioteState s))) m)

runSymbioteT :: forall s m
              . Monad m
             => SymbioteT s m Unit
             -> Boolean -- ^ Is this the first peer to initiate the protocol?
             -> m (Map Topic (Exists (SymbioteState s)))
runSymbioteT x isFirst = execStateT (runReaderT x isFirst) Map.empty

data GenerateSymbiote s
  = DoneGenerating
  | GeneratedSymbiote
    { generatedValue :: s
    , generatedOperation :: s
    }

generateSymbiote :: forall s m. MonadEffect m => Exists (SymbioteState s) -> m (GenerateSymbiote s)
generateSymbiote e = runExists inState e
  where
    inState :: forall a. SymbioteState s a -> m (GenerateSymbiote s)
    inState (SymbioteState {encode',generate,generateOp,maxSize,generation}) = do
      let go (SymbioteGeneration g@{size}) = SymbioteGeneration (g {size = size + 1})
      SymbioteGeneration{size} <- liftEffect $ Ref.modify go generation
      if size >= maxSize
        then pure DoneGenerating
        else do
          let genResize :: forall q. Gen q -> m q
              genResize x =
                unsafePartial $ liftEffect $ do
                  seed <- randomSeed
                  pure $ QC.evalGen x {newSeed: seed, size}
          generatedValue <- encode' <$> genResize generate
          generatedOperation <- genResize generateOp
          pure $ GeneratedSymbiote {generatedValue,generatedOperation}


getProgress :: forall s m. MonadEffect m => Exists (SymbioteState s) -> m Number
getProgress e = runExists inState e
  where
    inState :: forall a. SymbioteState s a -> m Number
    inState (SymbioteState {maxSize,generation}) = do
      SymbioteGeneration{size} <- liftEffect $ Ref.read generation
      pure $ Int.toNumber size / Int.toNumber maxSize
