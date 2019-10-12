module Test.Serialization.Symbiote.WebSocket where

import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, defaultProgress, Topic, Failure)

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, runAff_, makeAff, nonCanceler, forkAff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throwException, throw, error)
import Data.Maybe (Maybe (..))
import Data.Either (Either (Right))
import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer
  , encodeArrayBuffer, decodeArrayBuffer, class DynamicByteLength)
import Data.Generic.Rep (class Generic)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Queue.One (Queue, READ, WRITE)
import Queue.One (new,put,draw,on,del) as Q
import WebSocket (dimap', WebSocketsApp (..), Capabilities, dimapStringify, dimapJson, newWebSocketString, newWebSocketBinary)
import WebSocket.Extra (logConsole)
import Debug.Trace (traceM)



data Debug = Debug | NoDebug


secondPeerWebSocketArrayBuffer :: forall m stM s s'
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => Show s
                       => Generic s s'
                       => EncodeArrayBuffer s
                       => DecodeArrayBuffer s
                       => DynamicByteLength s
                       => String
                       -> Debug
                       -> SymbioteT s m Unit
                       -> m Unit
secondPeerWebSocketArrayBuffer host debug = peerWebSocketArrayBuffer host debug secondPeer

firstPeerWebSocketArrayBuffer :: forall m stM s s'
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => Show s
                       => Generic s s'
                       => EncodeArrayBuffer s
                       => DecodeArrayBuffer s
                       => DynamicByteLength s
                       => String
                       -> Debug
                       -> SymbioteT s m Unit
                       -> m Unit
firstPeerWebSocketArrayBuffer host debug = peerWebSocketArrayBuffer host debug firstPeer

secondPeerWebSocketJson :: forall m stM s s'
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => Show s
                       => Generic s s'
                       => EncodeJson s
                       => DecodeJson s
                       => String
                       -> Debug
                       -> SymbioteT s m Unit
                       -> m Unit
secondPeerWebSocketJson host debug = peerWebSocketJson host debug secondPeer

firstPeerWebSocketJson :: forall m stM s s'
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => Show s
                       => Generic s s'
                       => EncodeJson s
                       => DecodeJson s
                       => String
                       -> Debug
                       -> SymbioteT s m Unit
                       -> m Unit
firstPeerWebSocketJson host debug = peerWebSocketJson host debug firstPeer


peerWebSocketArrayBuffer :: forall m stM s them me
                          . MonadEffect m
                         => MonadAff m
                         => MonadBaseControl Aff m stM
                         => SingletonFunctor stM
                         => Show s
                         => EncodeArrayBuffer (me s)
                         => DecodeArrayBuffer (them s)
                         => DynamicByteLength (me s)
                         => String
                         -> Debug
                         -> ( (me s -> m Unit)
                           -> m (them s)
                           -> (Topic -> m Unit)
                           -> (Failure them s -> m Unit)
                           -> (Topic -> Number -> m Unit)
                           -> SymbioteT s m Unit
                           -> m Unit
                           )
                         -> SymbioteT s m Unit
                         -> m Unit
peerWebSocketArrayBuffer host debug = peerWebSocket \app ->
  newWebSocketBinary host []
    -- FIXME use AV?
    -- $ ( case debug of
    --       Debug -> logConsole
    --       NoDebug -> identity
    --   )
    $ dimap' receive send app
  where
    receive :: ArrayBuffer -> Effect (them s)
    receive buf = do
      mX <- decodeArrayBuffer buf
      case mX of
        Nothing -> do
          log "Can't parse buffer:"
          traceM buf
          throw "Failed."
        Just x -> pure x

    send :: me s -> ArrayBuffer
    send x = unsafePerformEffect do
      buf <- encodeArrayBuffer x
      pure buf


peerWebSocketJson :: forall m stM s them me
                   . MonadEffect m
                  => MonadAff m
                  => MonadBaseControl Aff m stM
                  => SingletonFunctor stM
                  => Show s
                  => EncodeJson (me s)
                  => DecodeJson (them s)
                  => String
                  -> Debug -- ^ Print incoming & outgoing?
                  -> ( (me s -> m Unit)
                    -> m (them s)
                    -> (Topic -> m Unit)
                    -> (Failure them s -> m Unit)
                    -> (Topic -> Number -> m Unit)
                    -> SymbioteT s m Unit
                    -> m Unit
                    )
                  -> SymbioteT s m Unit
                  -> m Unit
peerWebSocketJson host debug = peerWebSocket
  ( newWebSocketString host []
    <<< ( case debug of
            Debug -> logConsole
            NoDebug -> identity
        )
    <<< dimapStringify
    <<< dimapJson
  )


peerWebSocket :: forall m stM s them me
               . MonadEffect m
              => MonadAff m
              => MonadBaseControl Aff m stM
              => SingletonFunctor stM
              => Show s
              => ( WebSocketsApp Effect (them s) (me s)
                -> Effect Unit
                 )
              -> ( (me s -> m Unit)
                -> m (them s)
                -> (Topic -> m Unit)
                -> (Failure them s -> m Unit)
                -> (Topic -> Number -> m Unit)
                -> SymbioteT s m Unit
                -> m Unit
                 )
              -> SymbioteT s m Unit
              -> m Unit
peerWebSocket webSocket peer tests = do
  (outgoing :: Queue (read :: READ, write :: WRITE) (me s)) <- liftEffect Q.new
  (incoming :: Queue (read :: READ, write :: WRITE) (them s)) <- liftEffect Q.new
  (done :: Queue (read :: READ, write :: WRITE) Unit) <- liftEffect Q.new
  let encodeAndSend x = liftEffect (Q.put outgoing x)
      receiveAndDecode = liftAff (Q.draw incoming)
      onSuccess t = liftEffect $ log $ "Topic finished: " <> show t
      onFailure = liftEffect <<< defaultFailure
      onProgress t n = liftEffect (defaultProgress t n)

      onopen :: Capabilities Effect (me s) -> Effect Unit
      onopen {close,send} = do
        Q.on done \_ -> close
        Q.on outgoing send
      app :: WebSocketsApp Effect (them s) (me s)
      app = WebSocketsApp \env ->
        { onclose: \_ -> do
            Q.del outgoing
            Q.del done
        , onmessage: \_ -> Q.put incoming
        , onopen
        , onerror: throwException
        }
  mainThread <- liftBaseWith_ \runInBase -> forkAff (runInBase (liftEffect (webSocket app)))
  peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
  liftEffect (Q.put done unit)
  liftAff (joinFiber mainThread)
