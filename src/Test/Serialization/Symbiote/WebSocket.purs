module Test.Serialization.Symbiote.WebSocket where

import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, defaultProgress, nullProgress, Topic, Failure)
import Test.Serialization.Symbiote.Argonaut (ShowJson)

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Aff (Aff, forkAff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throwException, throw)
import Data.Maybe (Maybe (..))
import Data.Functor.Singleton (class SingletonFunctor, liftBaseWith_)
import Data.UInt (UInt)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.ArrayBuffer.Typed (whole, buffer)
import Data.ArrayBuffer.Typed.Unsafe (AV (..))
import Data.ArrayBuffer.Types (ArrayBuffer, Uint8)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer
  , encodeArrayBuffer, decodeArrayBuffer, class DynamicByteLength)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Queue.One (Queue, READ, WRITE)
import Queue.One (new,put,draw,on,del) as Q
import WebSocket (dimap', WebSocketsApp (..), Capabilities, dimapStringify, dimapJson, newWebSocketString, newWebSocketBinary)
import WebSocket.Extra (logConsole)
import Debug.Trace (traceM)



data Debug = FullDebug | Percent | NoDebug


secondPeerWebSocketArrayBuffer :: forall m stM
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => String
                       -> Debug
                       -> SymbioteT (AV Uint8 UInt) m Unit
                       -> m Unit
secondPeerWebSocketArrayBuffer host debug = peerWebSocketArrayBuffer host debug secondPeer

firstPeerWebSocketArrayBuffer :: forall m stM
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => String
                       -> Debug
                       -> SymbioteT (AV Uint8 UInt) m Unit
                       -> m Unit
firstPeerWebSocketArrayBuffer host debug = peerWebSocketArrayBuffer host debug firstPeer

secondPeerWebSocketJson :: forall m stM
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => String
                       -> Debug
                       -> SymbioteT ShowJson m Unit
                       -> m Unit
secondPeerWebSocketJson host debug = peerWebSocketJson host debug secondPeer

firstPeerWebSocketJson :: forall m stM
                        . MonadEffect m
                       => MonadAff m
                       => MonadBaseControl Aff m stM
                       => SingletonFunctor stM
                       => String
                       -> Debug
                       -> SymbioteT ShowJson m Unit
                       -> m Unit
firstPeerWebSocketJson host debug = peerWebSocketJson host debug firstPeer


peerWebSocketArrayBuffer :: forall m stM them me
                          . MonadEffect m
                         => MonadAff m
                         => MonadBaseControl Aff m stM
                         => SingletonFunctor stM
                         => EncodeArrayBuffer (me (AV Uint8 UInt))
                         => DecodeArrayBuffer (them (AV Uint8 UInt))
                         => DynamicByteLength (me (AV Uint8 UInt))
                         => String
                         -> Debug
                         -> ( (me (AV Uint8 UInt) -> m Unit)
                           -> m (them (AV Uint8 UInt))
                           -> (Topic -> m Unit)
                           -> (Failure them (AV Uint8 UInt) -> m Unit)
                           -> (Topic -> Number -> m Unit)
                           -> SymbioteT (AV Uint8 UInt) m Unit
                           -> m Unit
                           )
                         -> SymbioteT (AV Uint8 UInt) m Unit
                         -> m Unit
peerWebSocketArrayBuffer host debug = peerWebSocket go debug
  where
    toAV :: WebSocketsApp Effect ArrayBuffer ArrayBuffer
         -> WebSocketsApp Effect (AV Uint8 UInt) (AV Uint8 UInt)
    toAV = dimap' receive send
      where
        receive :: AV Uint8 UInt -> Effect ArrayBuffer
        receive (AV t) = pure (buffer t)
        send :: ArrayBuffer -> AV Uint8 UInt
        send b = unsafePerformEffect (AV <$> whole b)
    fromAV :: WebSocketsApp Effect (AV Uint8 UInt) (AV Uint8 UInt)
           -> WebSocketsApp Effect ArrayBuffer ArrayBuffer
    fromAV = dimap' receive send
      where
        receive :: ArrayBuffer -> Effect (AV Uint8 UInt)
        receive b = AV <$> whole b
        send :: AV Uint8 UInt -> ArrayBuffer
        send (AV t) = buffer t
    go app =
      newWebSocketBinary host []
        $ ( case debug of
              FullDebug -> fromAV <<< logConsole <<< toAV
              _ -> identity
          )
        $ dimap' receive send app
      where
        receive :: ArrayBuffer -> Effect (them (AV Uint8 UInt))
        receive buf = do
          mX <- decodeArrayBuffer buf
          case mX of
            Nothing -> do
              log "Can't parse buffer:"
              traceM buf
              throw "Failed."
            Just x -> pure x

        send :: me (AV Uint8 UInt) -> ArrayBuffer
        send x = unsafePerformEffect (encodeArrayBuffer x)


peerWebSocketJson :: forall m stM them me
                   . MonadEffect m
                  => MonadAff m
                  => MonadBaseControl Aff m stM
                  => SingletonFunctor stM
                  => EncodeJson (me ShowJson)
                  => DecodeJson (them ShowJson)
                  => String
                  -> Debug -- ^ Print incoming & outgoing?
                  -> ( (me ShowJson -> m Unit)
                    -> m (them ShowJson)
                    -> (Topic -> m Unit)
                    -> (Failure them ShowJson -> m Unit)
                    -> (Topic -> Number -> m Unit)
                    -> SymbioteT ShowJson m Unit
                    -> m Unit
                    )
                  -> SymbioteT ShowJson m Unit
                  -> m Unit
peerWebSocketJson host debug = peerWebSocket
  ( newWebSocketString host []
    <<< ( case debug of
            FullDebug -> logConsole
            _ -> identity
        )
    <<< dimapStringify
    <<< dimapJson
  )
  debug


peerWebSocket :: forall m stM s them me
               . MonadEffect m
              => MonadAff m
              => MonadBaseControl Aff m stM
              => SingletonFunctor stM
              => Show s
              => ( WebSocketsApp Effect (them s) (me s)
                -> Effect Unit
                 )
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
peerWebSocket webSocket debug peer tests = do
  (outgoing :: Queue (read :: READ, write :: WRITE) (me s)) <- liftEffect Q.new
  (incoming :: Queue (read :: READ, write :: WRITE) (them s)) <- liftEffect Q.new
  (done :: Queue (read :: READ, write :: WRITE) Unit) <- liftEffect Q.new
  let encodeAndSend x = liftEffect (Q.put outgoing x)
      receiveAndDecode = liftAff (Q.draw incoming)
      onSuccess t = liftEffect $ log $ "Topic finished: " <> show t
      onFailure = liftEffect <<< defaultFailure
      onProgress t n = case debug of
        NoDebug -> nullProgress t n
        _ -> liftEffect (defaultProgress t n)

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
