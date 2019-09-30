module Test.Serialization.Symbiote
  ( module Exposed
  , SimpleSerialization (..), register
  , firstPeer, secondPeer, First (..), Second (..), Generating (..), Operating (..), Failure (..)
  , defaultSuccess, defaultFailure, defaultProgress, nullProgress, simpleTest, simpleTest'
  ) where

import Test.Serialization.Symbiote.Core
  ( Topic (..), newGeneration, class Symbiote, encodeOp, decodeOp, perform, SymbioteT, runSymbioteT
  , SymbioteState (..), encode, decode, encodeOut, decodeOut
  , getProgress, generateSymbiote, GenerateSymbiote (..), ExistsSymbiote
  , mkExistsSymbiote, runExistsSymbiote)
import Test.Serialization.Symbiote.Core
  ( Topic (..), SymbioteT, class SymbioteOperation, perform, class Symbiote
  , encode, decode, encodeOp, decodeOp, encodeOut, decodeOut
  ) as Exposed

import Prelude
import Data.Map (Map)
import Data.Map (insert, keys, lookup) as Map
import Data.Set (findMax, delete) as Set
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Control.Monad.State (modify)
import Control.Monad.Trans.Control (class MonadBaseControl, liftBaseWith)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff, forkAff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import Effect.Console (log, error, warn)
import Queue.One (Queue, READ, WRITE)
import Queue.One (new, put, draw) as Queue
import Type.Proxy (Proxy (..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Partial.Unsafe (unsafePartial)
import Debug.Trace (traceM)


-- | The most trivial serialization medium for any @a@.
data SimpleSerialization a o op
  = SimpleValue a
  | SimpleOutput o
  | SimpleOperation op
derive instance genericSimpleSerialization :: (Generic a a', Generic o o', Generic op op') => Generic (SimpleSerialization a o op) _
instance eqSimpleSerialization ::
  ( Eq a, Eq o, Eq op
  ) => Eq (SimpleSerialization a o op) where
  eq x y = case Tuple x y of
    Tuple (SimpleValue a) (SimpleValue b) -> a == b
    Tuple (SimpleOutput a) (SimpleOutput b) -> a == b
    Tuple (SimpleOperation a) (SimpleOperation b) -> a == b
    _ -> false
instance showSimpleSerialization ::
  ( Show a, Show o, Show op
  ) => Show (SimpleSerialization a o op) where
  show x = case x of
    SimpleValue y -> "SimpleValue (" <> show y <> ")"
    SimpleOutput y -> "SimpleOutput (" <> show y <> ")"
    SimpleOperation y -> "SimpleOperation (" <> show y <> ")"

instance symbioteSimpleSerialization :: Exposed.SymbioteOperation a o op => Symbiote a o op (SimpleSerialization a o op) where
  encode = SimpleValue
  decode (SimpleValue x) = Just x
  decode _ = Nothing
  encodeOut _ = SimpleOutput
  decodeOut _ (SimpleOutput x) = Just x
  decodeOut _ _ = Nothing
  encodeOp = SimpleOperation
  decodeOp (SimpleOperation x) = Just x
  decodeOp _ = Nothing


-- | Register a topic in the test suite
register :: forall a o op s m
          . Arbitrary a
         => Arbitrary op
         => Symbiote a o op s
         => Eq o
         => MonadEffect m
         => Topic
         -> Int -- ^ Max size
         -> Proxy {value :: a, output :: o, operation :: op}
         -> SymbioteT s m Unit
register topic maxSize Proxy = do
  generation <- liftEffect (Ref.new newGeneration)
  let newState :: SymbioteState a o s
      newState = SymbioteState
        { generate: arbitrary
        , generateOp: encodeOp <$> (arbitrary :: _ op)
        , equal: (==)
        , maxSize
        , generation
        , encode': encode
        , encodeOut': encodeOut (Proxy :: Proxy a)
        , decode': decode
        , decodeOut': decodeOut (Proxy :: Proxy a)
        , perform': unsafePartial $ \op x -> case decodeOp op of
          Just (op' :: op) -> perform op' x
        }
  void (modify (Map.insert topic (mkExistsSymbiote newState)))

-- | Messages sent by a peer during their generating phase
data Generating s
  = Generated {value :: s, operation :: s}
  | BadResult s -- ^ Expected value
  | YourTurn
  | ImFinished
  | GeneratingNoParseOperated s
derive instance genericGenerating :: Generic s s' => Generic (Generating s) _
instance eqGenerating :: (Eq s, Generic s s') => Eq (Generating s) where
  eq = genericEq
instance showGenerating :: (Show s, Generic s s') => Show (Generating s) where
  show = genericShow


-- | Messages sent by a peer during their operating phase
data Operating s
  = Operated s -- ^ Serialized value after operation
  | OperatingNoParseValue s
  | OperatingNoParseOperation s
derive instance genericOperating :: Generic s s' => Generic (Operating s) _
instance eqOperating :: (Eq s, Generic s s') => Eq (Operating s) where
  eq = genericEq
instance showOperating :: (Show s, Generic s s') => Show (Operating s) where
  show = genericShow


-- | Messages sent by the first peer
data First s
  = AvailableTopics (Map Topic Int) -- ^ Mapping of topics to their gen size
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


-- | Messages sent by the second peer
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
defaultFailure :: forall them s
                . Failure them s -> Effect Unit
defaultFailure f = do
  warn "Failure:"
  traceM f
  error "Failed."

-- | Via putStrLn
defaultProgress :: Topic -> Number -> Effect Unit
defaultProgress (Topic t) p = log $ "Topic " <> t <> ": " <> show (p * 100.0) <> "%"

-- | Do nothing
nullProgress :: forall m. Applicative m => Topic -> Number -> m Unit
nullProgress _ _ = pure unit


-- | Run the test suite as the first peer
firstPeer :: forall m s
           . MonadEffect m
          => MonadAff m
          => Show s
          => (First s -> m Unit) -- ^ Encode and send first messages
          -> m (Second s) -- ^ Receive and decode second messages
          -> (Topic -> m Unit) -- ^ Report when Successful
          -> (Failure Second s -> m Unit) -- ^ Report when Failed
          -> (Topic -> Number -> m Unit) -- ^ Report on Progress
          -> SymbioteT s m Unit
          -> m Unit
firstPeer encodeAndSend receiveAndDecode onSuccess onFailure onProgress x = do
  state <- runSymbioteT x true
  let topics = (\e -> runExistsSymbiote (\(SymbioteState {maxSize}) -> maxSize) e) <$> state
  encodeAndSend (AvailableTopics topics)
  shouldBeStart <- receiveAndDecode
  case shouldBeStart of
    BadTopics badTopics -> onFailure $ BadTopicsFailure {first: topics, second: badTopics}
    Start -> do
      topicsToProcess <- liftEffect (Ref.new (Map.keys topics))
      let processAllTopics :: m Unit
          processAllTopics = do
            topics' <- liftEffect (Ref.read topicsToProcess)
            case Set.findMax topics' of
              Nothing -> pure unit -- done
              Just topic -> do
                let newTopics = Set.delete topic topics'
                liftEffect (Ref.write newTopics topicsToProcess)
                case Map.lookup topic state of
                  Nothing -> onFailure $ TopicNonexistent topic
                  Just symbioteState -> do
                    hasSentFinishedVar <- liftEffect (Ref.new HasntSentFinished)
                    hasReceivedFinishedVar <- liftEffect (Ref.new HasntReceivedFinished)
                    generating
                      encodeAndSend receiveAndDecode
                      (\topic' generating' -> FirstGenerating {topic:topic',generating:generating'})
                      (\topic' operating' -> FirstOperating {topic:topic',operating:operating'})
                      getSecondGenerating getSecondOperating
                      hasSentFinishedVar hasReceivedFinishedVar
                      processAllTopics
                      onSuccess
                      onFailure
                      onProgress
                      topic symbioteState
      processAllTopics
    _ -> onFailure $ OutOfSyncSecond shouldBeStart


-- | Run the test suite as the second peer
secondPeer :: forall s m
            . MonadEffect m
           => MonadAff m
           => Show s
           => (Second s -> m Unit) -- ^ Encode and send second messages
           -> m (First s) -- ^ Receive and decode first messages
           -> (Topic -> m Unit) -- ^ Report when Successful
           -> (Failure First s -> m Unit) -- ^ Report when Failed
           -> (Topic -> Number -> m Unit) -- ^ Report on Progress
           -> SymbioteT s m Unit
           -> m Unit
secondPeer encodeAndSend receiveAndDecode onSuccess onFailure onProgress x = do
  state <- runSymbioteT x false
  shouldBeAvailableTopics <- receiveAndDecode
  case shouldBeAvailableTopics of
    AvailableTopics topics -> do
      let myTopics = (\e -> runExistsSymbiote (\(SymbioteState {maxSize}) -> maxSize) e) <$> state
      if myTopics /= topics
        then do
          encodeAndSend (BadTopics myTopics)
          onFailure $ BadTopicsFailure {first: topics, second: myTopics}
        else do
          encodeAndSend Start
          topicsToProcess <- liftEffect (Ref.new (Map.keys topics))
          let processAllTopics :: m Unit
              processAllTopics = do
                topics' <- liftEffect (Ref.read topicsToProcess)
                case Set.findMax topics' of
                  Nothing -> pure unit -- done
                  Just topic -> do
                    let newTopics = Set.delete topic topics'
                    liftEffect (Ref.write newTopics topicsToProcess)
                    case Map.lookup topic state of
                      Nothing -> onFailure $ TopicNonexistent topic
                      Just symbioteState -> do
                        hasSentFinishedVar <- liftEffect (Ref.new HasntSentFinished)
                        hasReceivedFinishedVar <- liftEffect (Ref.new HasntReceivedFinished)
                        operating
                          encodeAndSend receiveAndDecode
                          (\topic' generating' -> SecondGenerating {topic:topic',generating:generating'})
                          (\topic' operating' -> SecondOperating {topic:topic',operating:operating'})
                          getFirstGenerating getFirstOperating
                          hasSentFinishedVar hasReceivedFinishedVar
                          processAllTopics
                          onSuccess
                          onFailure
                          onProgress
                          topic symbioteState
          processAllTopics
    _ -> onFailure $ OutOfSyncFirst shouldBeAvailableTopics



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
           -> ExistsSymbiote s
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
  topic symbioteState = runExistsSymbiote go symbioteState
  where
    go :: forall a o. SymbioteState a o s -> m Unit
    go (SymbioteState{equal,encode',decode',perform',decodeOut',encodeOut'}) = do
      mGenerated <- generateSymbiote symbioteState
      case mGenerated of
        DoneGenerating -> do
          encodeAndSend (makeGen topic ImFinished)
          liftEffect (Ref.write HasSentFinished hasSentFinishedVar)
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
                  Operated operatedValueEncoded -> case decodeOut' operatedValueEncoded of
                    Nothing -> do
                      encodeAndSend $ makeGen topic $ GeneratingNoParseOperated operatedValueEncoded
                      onFailure $ CantParseOperated topic operatedValueEncoded
                    Just operatedValue -> case decode' generatedValueEncoded of
                      Nothing -> onFailure $ CantParseLocalValue topic generatedValueEncoded
                      Just generatedValue -> do
                        -- decoded operated value, generated value & operation
                        let expected :: o
                            expected = perform' generatedOperationEncoded generatedValue
                        if  equal expected operatedValue
                          then do
                            encodeAndSend (makeGen topic YourTurn)
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
                            onFailure $ SafeFailure {topic, expected: encodeOut' expected, got: operatedValueEncoded}
                  _ -> onFailure $ BadOperating topic shouldBeOperated
            _ -> onFailure $ BadThem topic shouldBeOperating
      where
        operatingTryFinished :: m Unit
        operatingTryFinished = do
          hasReceivedFinished <- liftEffect (Ref.read hasReceivedFinishedVar)
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
          -> ExistsSymbiote s
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
  topic symbioteState = runExistsSymbiote go symbioteState
  where
    go :: forall a o. SymbioteState a o s -> m Unit
    go (SymbioteState{decode',perform',encode',encodeOut'}) = do
      shouldBeGenerating <- receiveAndDecode
      case getGen shouldBeGenerating of
        Just {topic:secondGeneratingTopic,generating:shouldBeGenerated}
          | secondGeneratingTopic /= topic ->
            onFailure $ WrongTopic {expected: topic, got: secondGeneratingTopic}
          | otherwise -> case shouldBeGenerated of
              ImFinished -> do
                liftEffect (Ref.write HasReceivedFinished hasReceivedFinishedVar)
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
                  encodeAndSend $ makeOp topic $ Operated $ encodeOut' $ perform' generatedOperationEncoded generatedValue
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
        generatingTryFinished :: m Unit
        generatingTryFinished = do
          hasSentFinished <- liftEffect (Ref.read hasSentFinishedVar)
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


-- | Prints to stdout and uses a local channel for a sanity-check - doesn't serialize.
simpleTest :: forall s m stM
            . MonadBaseControl Aff m stM
           => MonadAff m
           => MonadEffect m
           => Show s
           => SymbioteT s m Unit
           -> m Unit
simpleTest = simpleTest'
  (const (pure unit))
  (liftEffect <<< defaultFailure)
  (liftEffect <<< defaultFailure)
  nullProgress

simpleTest' :: forall s m stM
             . MonadBaseControl Aff m stM
            => MonadAff m
            => MonadEffect m
            => Show s
            => (Topic -> m Unit) -- ^ report topic success
            -> (Failure Second s -> m Unit) -- ^ report topic failure
            -> (Failure First s -> m Unit) -- ^ report topic failure
            -> (Topic -> Number -> m Unit) -- ^ report topic progress
            -> SymbioteT s m Unit
            -> m Unit
simpleTest' onSuccess onFailureSecond onFailureFirst onProgress suite = do
  firstChan <- liftEffect Queue.new
  secondChan <- liftEffect Queue.new

  t <- liftBaseWith $ \runInBase -> forkAff $
    void $ runInBase $ firstPeer
      (encodeAndSendChan firstChan)
      (receiveAndDecodeChan secondChan)
      onSuccess onFailureSecond onProgress
      suite
  secondPeer
    (encodeAndSendChan secondChan)
    (receiveAndDecodeChan firstChan)
    onSuccess onFailureFirst onProgress
    suite
  liftAff (joinFiber t)
  where
    encodeAndSendChan :: forall a. Queue (read :: READ, write :: WRITE) a -> a -> m Unit
    encodeAndSendChan chan x = liftEffect (Queue.put chan x)
    receiveAndDecodeChan :: forall a. Queue (read :: READ, write :: WRITE) a -> m a
    receiveAndDecodeChan chan = liftAff (Queue.draw chan)
