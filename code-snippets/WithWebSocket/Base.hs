{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module WithWebSocket.Base
  (withWSConnection, WithWebSocketT)
  where

import Reflex.Dom
import Data.Aeson
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import WithWebSocket.Class

import Data.ByteString
import Data.Text
import qualified Data.ByteString.Lazy as BSL

type WithWebSocketT t m = RequesterT t HasToJSON HasFromJSON m

instance PrimMonad m => PrimMonad (WithWebSocketT x m) where
  type PrimState (WithWebSocketT x m) = PrimState m
  primitive = lift . primitive

instance
     (DomBuilder t m, PostBuild t m, PrimMonad m,
     MonadFix m, HasWebView m, MonadHold t m,
     PerformEvent t m, MonadIO m, MonadIO (Performable m),
     TriggerEvent t m)
  => WithWebSocket t (WithWebSocketT t m) where
  getWebSocketResponse req = do
    resp <- requesting $ HasToJSON <$> req
    return $ (\(HasFromJSON b) -> b) <$> resp

data HasToJSON a where
  HasToJSON :: (ToJSON a) => a -> HasToJSON a
data HasFromJSON a where
  HasFromJSON :: (FromJSON (WebSocketResponseType a)) => (WebSocketResponseType a) -> HasFromJSON a

withWSConnection ::
  (DomBuilder t m, PostBuild t m, PrimMonad m,
   MonadFix m, HasWebView m, MonadHold t m,
   PerformEvent t m, MonadIO m, MonadIO (Performable m),
   TriggerEvent t m)
  => Text -- URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect
  -> WithWebSocketT t m a
  -> m (a, WebSocket t)
withWSConnection url closeEv reconnect wdgt = do
  rec
    let
      recvEv = _webSocket_recv ws
    -- (val,_, evList) <- runRWST (unWithWebSocketT wdgt) recvEv ()
      evList = never :: Reflex t => (Event t [ByteString])
      recvEvMap = never
    (val, sendEvMap) <- runRequesterT wdgt recvEvMap
    let
      -- sendEv = NE.toList <$> mergeList evList
      sendEv = evList
      conf = WebSocketConfig sendEv closeEv reconnect
    ws <- webSocket url conf
  return (val, ws)
