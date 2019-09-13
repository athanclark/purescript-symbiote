module Test.Serialization.Symbiote where

import Test.Serialization.Symbiote.Core
  ( Topic (..), newGeneration, class Symbiote, encodeOp, decodeOp, perform, SymbioteT
  , SymbioteState (..), encode, decode, getProgress, generateSymbiote, GenerateSymbiote (..))

import Prelude
import Data.Exists (Exists, mkExists, runExists)
import Data.Map (Map)
import Data.Map (insert) as Map
import Data.Maybe (Maybe (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Control.Monad.State (modify)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Effect.Console (log, error)
import Type.Proxy (Proxy (..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Partial.Unsafe (unsafePartial)


register :: forall a op s m
          . Arbitrary a
         => Arbitrary op
         => Symbiote a op s
         => Eq a
         => MonadEffect m
         => Topic
         -> Int -- ^ Max size
         -> Proxy {value :: a, operation :: op}
         -> SymbioteT s m Unit
register topic maxSize Proxy = do
  generation <- liftEffect (Ref.new newGeneration)
  let newState :: SymbioteState s a
      newState = SymbioteState
        { generate: arbitrary
        , generateOp: encodeOp <$> (arbitrary :: _ op)
        , equal: (==)
        , maxSize
        , generation
        , encode': encode
        , decode': decode
        , perform': unsafePartial $ \op x -> case decodeOp op of
          Just (op' :: op) -> perform op' x
        }
  void (modify (Map.insert topic (mkExists newState)))

data Generating s
  = Generated {value :: s, operation :: s}
  | BadResult s
  | YourTurn
  | ImFinished
  | GeneratingNoParseOperated s
derive instance genericGenerating :: Generic s s' => Generic (Generating s) _
instance eqGenerating :: (Eq s, Generic s s') => Eq (Generating s) where
  eq = genericEq
instance showGenerating :: (Show s, Generic s s') => Show (Generating s) where
  show = genericShow


data Operating s
  = Operated s
  | OperatingNoParseValue s
  | OperatingNoParseOperation s
derive instance genericOperating :: Generic s s' => Generic (Operating s) _
instance eqOperating :: (Eq s, Generic s s') => Eq (Operating s) where
  eq = genericEq
instance showOperating :: (Show s, Generic s s') => Show (Operating s) where
  show = genericShow


data First s
  = AvailableTopics (Map Topic Int)
  | FirstGenerating {topic :: Topic, generating :: Generating s}
  | FirstOperating {topic :: Topic, operating :: Operating s}
derive instance genericFirst :: Generic s s' => Generic (First s) _
instance eqFirst :: (Eq s, Generic s s') => Eq (First s) where
  eq = genericEq
instance showFirst :: (Show s, Generic s s') => Show (First s) where
  show = genericShow

getFirstGenerating :: forall s. First s -> Maybe {topic :: Topic, generating :: Generating s}
getFirstGenerating x = case x of
  FirstGenerating y -> Just y
  _ -> Nothing

getFirstOperating :: forall s. First s -> Maybe {topic :: Topic, operating :: Operating s}
getFirstOperating x = case x of
  FirstOperating y -> Just y
  _ -> Nothing


data Second s
  = BadTopics (Map Topic Int)
  | Start
  | SecondOperating {topic :: Topic, operating :: Operating s}
  | SecondGenerating {topic :: Topic, generating :: Generating s}
derive instance genericSecond :: Generic s s' => Generic (Second s) _
instance eqSecond :: (Eq s, Generic s s') => Eq (Second s) where
  eq = genericEq
instance showSecond :: (Show s, Generic s s') => Show (Second s) where
  show = genericShow

getSecondGenerating :: forall s. Second s -> Maybe {topic :: Topic, generating :: Generating s}
getSecondGenerating x = case x of
  SecondGenerating y -> Just y
  _ -> Nothing

getSecondOperating :: forall s. Second s -> Maybe {topic :: Topic, operating :: Operating s}
getSecondOperating x = case x of
  SecondOperating y -> Just y
  _ -> Nothing


data Failure them s
  = BadTopicsFailure
    { first :: Map Topic Int
    , second :: Map Topic Int
    }
  | OutOfSyncFirst (First s)
  | OutOfSyncSecond (Second s)
  | TopicNonexistent Topic
  | WrongTopic
    { expected :: Topic
    , got :: Topic
    }
  | CantParseOperated Topic s
  | CantParseGeneratedValue Topic s
  | CantParseGeneratedOperation Topic s
  | CantParseLocalValue Topic s
  | CantParseLocalOperation Topic s
  | BadOperating Topic (Operating s)
  | BadGenerating Topic (Generating s)
  | BadThem Topic (them s)
  | SafeFailure
    { topic :: Topic
    , expected :: s
    , got :: s
    }
derive instance genericFailure :: (Generic s s', Generic (them s) them') => Generic (Failure them s) _
instance eqFailure :: (Eq s, Generic s s', Eq (them s), Generic (them s) them') => Eq (Failure them s) where
  eq = genericEq
instance showFailure :: (Show s, Generic s s', Show (them s), Generic (them s) them') => Show (Failure them s) where
  show = genericShow

-- | Via putStrLn
defaultSuccess :: Topic -> Effect Unit
defaultSuccess (Topic t) = log $ "Topic " <> t <> " succeeded"

-- | Via putStrLn
defaultFailure :: forall them s them' s'
                . Show (them s)
               => Generic (them s) them'
               => Show s
               => Generic s s'
               => Failure them s -> Effect Unit
defaultFailure f = error $ "Failure: " <> show f

-- | Via putStrLn
defaultProgress :: Topic -> Number -> Effect Unit
defaultProgress (Topic t) p = log $ "Topic " <> t <> ": " <> {- printf "%.2f" -} show (p * 100.0) <> "%"

-- | Do nothing
nullProgress :: Topic -> Number -> Effect Unit
nullProgress _ _ = pure unit


data HasSentFinished
  = HasSentFinished
  | HasntSentFinished

data HasReceivedFinished
  = HasReceivedFinished
  | HasntReceivedFinished


generating :: forall s m them me
            . MonadAff m
           => Show s
           => (me s -> m Unit) -- ^ Encode and send first messages
           -> m (them s) -- ^ Receive and decode second messages
           -> (Topic -> Generating s -> me s) -- ^ Build a generating datum, whether first or second
           -> (Topic -> Operating s -> me s) -- ^ Build a generating datum, whether first or second
           -> (them s -> Maybe {topic :: Topic, generating :: Generating s}) -- ^ Deconstruct an operating datum, whether first or second
           -> (them s -> Maybe {topic :: Topic, operating :: Operating s}) -- ^ Deconstruct an operating datum, whether first or second
           -> Ref HasSentFinished
           -> Ref HasReceivedFinished
           -> m Unit -- ^ on finished - loop
           -> (Topic -> m Unit) -- ^ report topic success
           -> (Failure them s -> m Unit) -- ^ report topic failure
           -> (Topic -> Number -> m Unit) -- ^ report topic progress
           -> Topic
           -> Exists (SymbioteState s)
           -> m Unit
generating
  encodeAndSend receiveAndDecode
  makeGen makeOp
  getGen getOp
  hasSentFinishedVar hasReceivedFinishedVar
  onFinished
  onSuccess
  onFailure
  onProgress
  topic symbioteState = runExists go symbioteState
  where
    go :: forall a. SymbioteState s a -> m Unit
    go (SymbioteState{equal,encode',decode',perform'}) = do
      mGenerated <- generateSymbiote symbioteState
      case mGenerated of
        DoneGenerating -> do
          encodeAndSend $ makeGen topic ImFinished
          liftEffect $ Ref.write HasSentFinished hasSentFinishedVar
          operatingTryFinished
        GeneratedSymbiote
          { generatedValue: generatedValueEncoded
          , generatedOperation: generatedOperationEncoded
          } -> do
          -- send
          encodeAndSend $ makeGen topic $ Generated
            { value: generatedValueEncoded
            , operation: generatedOperationEncoded
            }
          -- receive
          shouldBeOperating <- receiveAndDecode
          case getOp shouldBeOperating of
            Just {topic:secondOperatingTopic, operating:shouldBeOperated}
              | secondOperatingTopic /= topic ->
                onFailure $ WrongTopic {expected: topic, got: secondOperatingTopic}
              | otherwise -> case shouldBeOperated of
                  Operated operatedValueEncoded -> case decode' operatedValueEncoded of
                    Nothing -> do
                      encodeAndSend $ makeGen topic $ GeneratingNoParseOperated operatedValueEncoded
                      onFailure $ CantParseOperated topic operatedValueEncoded
                    Just operatedValue -> case decode' generatedValueEncoded of
                      Nothing -> onFailure $ CantParseLocalValue topic generatedValueEncoded
                      Just generatedValue -> do
                        -- decoded operated value, generated value & operation
                        let expected = perform' generatedOperationEncoded generatedValue
                        if equal expected operatedValue
                          then do
                            encodeAndSend $ makeGen topic YourTurn
                            progress <- getProgress symbioteState
                            onProgress topic progress
                            operating
                              encodeAndSend receiveAndDecode
                              makeGen makeOp
                              getGen getOp
                              hasSentFinishedVar hasReceivedFinishedVar
                              onFinished
                              onSuccess
                              onFailure
                              onProgress
                              topic symbioteState
                          else do
                            encodeAndSend $ makeGen topic $ BadResult operatedValueEncoded
                            onFailure $ SafeFailure {topic, expected: encode' expected, got: operatedValueEncoded}
                  _ -> onFailure $ BadOperating topic shouldBeOperated
            _ -> onFailure $ BadThem topic shouldBeOperating
      where
        operatingTryFinished = do
          hasReceivedFinished <- liftEffect $ Ref.read hasReceivedFinishedVar
          case hasReceivedFinished of
            HasReceivedFinished -> do
              onSuccess topic
              onFinished -- stop cycling - last generation in sequence is from second
            HasntReceivedFinished -> do
              progress <- getProgress symbioteState
              onProgress topic progress
              operating
                encodeAndSend receiveAndDecode
                makeGen makeOp
                getGen getOp
                hasSentFinishedVar hasReceivedFinishedVar
                onFinished
                onSuccess
                onFailure
                onProgress
                topic symbioteState




operating :: forall s m them me
           . MonadAff m
          => Show s
          => (me s -> m Unit) -- ^ Encode and send first messages
          -> m (them s) -- ^ Receive and decode second messages
          -> (Topic -> Generating s -> me s) -- ^ Build a generating datum, whether first or second
          -> (Topic -> Operating s -> me s) -- ^ Build a generating datum, whether first or second
          -> (them s -> Maybe {topic :: Topic, generating :: Generating s}) -- ^ Deconstruct an operating datum, whether first or second
          -> (them s -> Maybe {topic :: Topic, operating :: Operating s}) -- ^ Deconstruct an operating datum, whether first or second
          -> Ref HasSentFinished
          -> Ref HasReceivedFinished
          -> m Unit -- ^ on finished
          -> (Topic -> m Unit) -- ^ report topic success
          -> (Failure them s -> m Unit) -- ^ report topic failure
          -> (Topic -> Number -> m Unit) -- ^ report topic progress
          -> Topic
          -> Exists (SymbioteState s)
          -> m Unit
operating
  encodeAndSend receiveAndDecode
  makeGen makeOp
  getGen getOp
  hasSentFinishedVar hasReceivedFinishedVar
  onFinished
  onSuccess
  onFailure
  onProgress
  topic symbioteState = runExists go symbioteState
  where
    go :: forall a. SymbioteState s a -> m Unit
    go (SymbioteState{decode',perform',encode'}) = do
      shouldBeGenerating <- receiveAndDecode
      case getGen shouldBeGenerating of
        Just {topic:secondGeneratingTopic,generating:shouldBeGenerated}
          | secondGeneratingTopic /= topic ->
            onFailure $ WrongTopic {expected: topic, got: secondGeneratingTopic}
          | otherwise -> case shouldBeGenerated of
              ImFinished -> do
                liftEffect $ Ref.write HasReceivedFinished hasReceivedFinishedVar
                generatingTryFinished
              YourTurn -> do
                progress <- getProgress symbioteState
                onProgress topic progress
                generating
                  encodeAndSend receiveAndDecode
                  makeGen makeOp
                  getGen getOp
                  hasSentFinishedVar hasReceivedFinishedVar
                  onFinished
                  onSuccess
                  onFailure
                  onProgress
                  topic symbioteState
              Generated
                { value: generatedValueEncoded
                , operation: generatedOperationEncoded
                } -> case decode' generatedValueEncoded of
                Nothing -> do
                  encodeAndSend $ makeOp topic $ OperatingNoParseValue generatedValueEncoded
                  onFailure $ CantParseGeneratedValue topic generatedValueEncoded
                Just (generatedValue :: a) -> do
                  encodeAndSend $ makeOp topic $ Operated $ encode' $ perform' generatedOperationEncoded generatedValue
                  -- wait for response
                  operating
                    encodeAndSend
                    receiveAndDecode
                    makeGen makeOp
                    getGen getOp
                    hasSentFinishedVar hasReceivedFinishedVar
                    onFinished
                    onSuccess
                    onFailure
                    onProgress
                    topic symbioteState
              _ -> onFailure $ BadGenerating topic shouldBeGenerated
        _ -> onFailure $ BadThem topic shouldBeGenerating
      where
        generatingTryFinished = do
          hasSentFinished <- liftEffect $ Ref.read hasSentFinishedVar
          case hasSentFinished of
            HasSentFinished -> do
              onSuccess topic
              onFinished -- stop cycling - last operation in sequence is from first
            HasntSentFinished -> do
              progress <- getProgress symbioteState
              onProgress topic progress
              generating
                encodeAndSend receiveAndDecode
                makeGen makeOp
                getGen getOp
                hasSentFinishedVar hasReceivedFinishedVar
                onFinished
                onSuccess
                onFailure
                onProgress
                topic symbioteState
