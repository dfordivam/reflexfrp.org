-- Doing websocket requests from multiple places over a single connection
-- using the Requester class
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Reflex.Dom
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString
import Data.Text
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Reflex.Requester.Class
import Reflex.Requester.Base

type family WebSocketResponseType a :: *
class
     (DomBuilder t m, PostBuild t m, PrimMonad m,
     MonadFix m, HasWebView m, MonadHold t m,
     PerformEvent t m, MonadIO m, MonadIO (Performable m),
     TriggerEvent t m)
  => WithWebSocket t m | m -> t where
  getWebSocketResponse ::
    (ToJSON a, FromJSON (WebSocketResponseType a),
     DomBuilder t m, PostBuild t m, PrimMonad m,
     MonadFix m, HasWebView m, MonadHold t m,
     PerformEvent t m, MonadIO m, MonadIO (Performable m),
     TriggerEvent t m, WithWebSocket t m)
     => Event t a -> m (Event t (WebSocketResponseType a))

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

-- type family WebSocketResponseType a :: *


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

-- Example Code
type instance WebSocketResponseType String = Char
type instance WebSocketResponseType Char = Int

codeToRun ::
  (DomBuilder t m, PostBuild t m, PrimMonad m,
   MonadFix m, HasWebView m, MonadHold t m,
   PerformEvent t m, MonadIO m, MonadIO (Performable m),
   TriggerEvent t m, WithWebSocket t m)
  => m ()
codeToRun = do
  ev <- button "hello"
  respEv <- getWebSocketResponse ('f' <$ ev)
  d <- holdDyn 0 respEv
  display d
  return ()

myWidget ::
  (DomBuilder t m, PostBuild t m, PrimMonad m,
   MonadFix m, HasWebView m, MonadHold t m,
   PerformEvent t m, MonadIO m, MonadIO (Performable m),
   TriggerEvent t m)
  => m ()
myWidget = do
  text "Test WithWebsocket"
  (_,_) <- withWSConnection "ws://echo.websocket.org/" never False codeToRun
  return ()

main = mainWidget myWidget
