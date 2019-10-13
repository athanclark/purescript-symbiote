module Test.Serialization.Symbiote.ZeroMQ where

import Test.Serialization.Symbiote
  (firstPeer, secondPeer, SymbioteT, defaultFailure, defaultProgress, nullProgress, Topic, Failure)
import Test.Serialization.Symbiote.Debug (Debug (..))

import Prelude
import Data.UInt (UInt)
import Data.Maybe (Maybe (..))
import Data.NonEmpty (NonEmpty (..))
import Data.Functor.Singleton (class SingletonFunctor)
import Data.ArrayBuffer.Typed (whole)
import Data.ArrayBuffer.Typed.Unsafe (AV (..))
import Data.ArrayBuffer.Types (Uint8, Uint8Array)
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer, class DecodeArrayBuffer
  , encodeArrayBuffer, decodeArrayBuffer, class DynamicByteLength)
import Control.Monad.Trans.Control (class MonadBaseControl)
import Effect.Console (log)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff (Aff, forkAff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (throw)
import ZeroMQ (socket, connect, sendMany, readMany, pair, close)
import Node.Buffer (toArrayBuffer, fromArrayBuffer)
import Queue.One (Queue, READ, WRITE)
import Queue.One (new, on, draw, del, put) as Q


secondPeerZeroMQ :: forall m stM
                  . MonadEffect m
                 => MonadAff m
                 => MonadBaseControl Aff m stM
                 => SingletonFunctor stM
                 => String
                 -> Debug
                 -> SymbioteT (AV Uint8 UInt) m Unit
                 -> m Unit
secondPeerZeroMQ host debug = peerZeroMQ host debug secondPeer

firstPeerZeroMQ :: forall m stM
                 . MonadEffect m
                => MonadAff m
                => MonadBaseControl Aff m stM
                => SingletonFunctor stM
                => String
                -> Debug
                -> SymbioteT (AV Uint8 UInt) m Unit
                -> m Unit
firstPeerZeroMQ host debug = peerZeroMQ host debug firstPeer

peerZeroMQ :: forall m stM them me
            . MonadEffect m
           => MonadAff m
           => MonadBaseControl Aff m stM
           => SingletonFunctor stM
           => EncodeArrayBuffer (me (AV Uint8 UInt))
           => DynamicByteLength (me (AV Uint8 UInt))
           => DecodeArrayBuffer (them (AV Uint8 UInt))
           => String -- ^ Host
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
peerZeroMQ host debug peer tests = do
  (outgoing :: Queue (read :: READ, write :: WRITE) (me (AV Uint8 UInt))) <- liftEffect Q.new
  (incoming :: Queue (read :: READ, write :: WRITE) (them (AV Uint8 UInt))) <- liftEffect Q.new
  (done :: Queue (read :: READ, write :: WRITE) Unit) <- liftEffect Q.new
  let encodeAndSend :: me (AV Uint8 UInt) -> m Unit
      encodeAndSend x = liftEffect (Q.put outgoing x)
      receiveAndDecode :: m (them (AV Uint8 UInt))
      receiveAndDecode = liftAff (Q.draw incoming)
      onSuccess :: Topic -> m Unit
      onSuccess t = liftEffect $ log $ "Topic finished: " <> show t
      onFailure :: Failure them (AV Uint8 UInt) -> m Unit
      onFailure = liftEffect <<< defaultFailure
      onProgress :: Topic -> Number -> m Unit
      onProgress t n = case debug of
        NoDebug -> nullProgress t n
        _ -> liftEffect (defaultProgress t n)

  mainThread <- liftAff $ forkAff do
    s <- liftEffect $ socket pair pair
    liftEffect $ connect s host
    liftEffect $ Q.on outgoing \x -> do
      ab <- encodeArrayBuffer x
      buf <- fromArrayBuffer ab
      sendMany unit s (NonEmpty buf [])
    liftEffect $ Q.on done \_ -> do
      Q.del outgoing
      close s
    let loop :: Aff Unit
        loop = do
          mX <- readMany s
          case mX of
            Nothing -> liftEffect (log "nothin")
            Just {msg: NonEmpty buf _} -> do
              ab <- liftEffect $ toArrayBuffer buf
              mX' <- liftEffect $ decodeArrayBuffer ab
              case mX' of
                Nothing -> liftEffect $ do
                  (ta :: Uint8Array) <- whole ab
                  throw $ "couldn't decode: " <> show ((AV ta) :: AV Uint8 UInt)
                Just x' -> do
                  liftEffect $ Q.put incoming x'
                  loop
    loop
  peer encodeAndSend receiveAndDecode onSuccess onFailure onProgress tests
  liftEffect (Q.put done unit)
  liftAff (joinFiber mainThread)
