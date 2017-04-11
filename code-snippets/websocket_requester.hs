-- Doing websocket requests from multiple places over a single connection
-- using the Requester class
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

import Reflex.Dom
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString
import Data.Text
import Control.Monad.Fix
import Control.Monad.IO.Class
import Reflex.Requester.Class
import Reflex.Requester.Base

type family Req :: * -> *
type family Resp :: * -> *
type WithWebSocketT t m a = RequesterT t Req Resp m a

codeToRun ::
  (DomBuilder t m, PostBuild t m,
   MonadFix m, HasWebView m, MonadHold t m,
   PerformEvent t m, MonadIO m, MonadIO (Performable m),
   TriggerEvent t m)
  => WithWebSocketT t m ()
codeToRun = do
  ev <- button "hello"
  -- let ev2 = Message <$ ev
  -- resp <- getResponse ev2
  -- let ev3 = showResp <$> resp
  return ()

withWSConnection ::
  (DomBuilder t m, PostBuild t m,
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

    (val, _) <- runRequesterT wdgt never
    let
      -- sendEv = NE.toList <$> mergeList evList
      sendEv = evList
      conf = WebSocketConfig sendEv closeEv reconnect
    ws <- webSocket url conf
  return (val, ws)

myWidget ::
  (DomBuilder t m, PostBuild t m,
   MonadFix m, HasWebView m, MonadHold t m,
   PerformEvent t m, MonadIO m, MonadIO (Performable m),
   TriggerEvent t m)
  => m ()
myWidget = do
  text "Test WithWebsocket"
  (_,_) <- withWSConnection "ws://echo.websocket.org/" never False codeToRun
  return ()

main = mainWidget myWidget
