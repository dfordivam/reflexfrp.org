-- Doing websocket requests from multiple places over a single connection
-- using the Requester class
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import Reflex.Dom
import Data.Aeson
import Data.Text
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Reflex.Requester.Class
import Reflex.Requester.Base

import WithWebSocket.Class
import WithWebSocket.Base

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
