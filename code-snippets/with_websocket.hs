{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import Reflex.Dom
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString
import Control.Monad.RWS.Class
import Control.Monad.RWS
import Control.Monad.Trans.Control
import Data.Text
import Data.List.NonEmpty as NE

import GHC.Generics

-- Ideal interface for getting the websocket response
class (DomBuilder t m)
  => WithWebSocket t m where
  getResponse :: (ToJSON req, FromJSON resp)
    => Event t req -> m (Event t resp)

newtype WithWebSocketT t m a =
  WithWebSocketT
  { unWithWebSocketT :: RWST (Event t ByteString) (Event t [ByteString]) () m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans,
           MonadReader (Event t ByteString), MonadWriter (Event t [ByteString]))

instance (DomBuilder t m) => WithWebSocket t (WithWebSocketT t m) where
  getResponse req = do
    -- (WithWebSocketT rwst)
    -- Add Event t ByteString for sendEv
    tell ((:[]) <$> BSL.toStrict <$> encode <$> req)
    -- Provide the response
    ev <- ask
    return $ fforMaybe ev decodeStrict

instance (Reflex t) => MonadTransControl (WithWebSocketT t) where
  type StT (WithWebSocketT t) a = StT (RWST (Event t ByteString) (Event t [ByteString]) ()) a
  liftWith = defaultLiftWith WithWebSocketT unWithWebSocketT
  restoreT = defaultRestoreT WithWebSocketT

instance DomBuilder t m => DomBuilder t (WithWebSocketT t m) where
  type DomBuilderSpace (WithWebSocketT t m) = DomBuilderSpace m
  {-# INLINABLE element #-}
  element t cfg child = do
    (ret, w) <- liftWith $ \run -> do
      (a,_,w) <- run child
      ret <- element t (liftElementConfig cfg) (pure a)
      return (ret, w)
    tell w
    return ret

  {-# INLINABLE selectElement #-}
  selectElement cfg child = do
    (ret, w) <- liftWith $ \run -> do
      let cfg' = cfg
                  { _selectElementConfig_elementConfig =
                    liftElementConfig $ _selectElementConfig_elementConfig cfg
                  }
      (a,_,w) <- run child
      ret <- selectElement cfg' (pure a)
      return (ret, w)
    tell w
    return ret

instance (MonadAdjust t m) => MonadAdjust t (WithWebSocketT t m)
  -- runWithReplace a0 a' = InputDisabledT $ runWithReplace (coerce a0) (coerceEvent a')
  -- traverseDMapWithKeyWithAdjust f dm0 dm' = InputDisabledT $ traverseDMapWithKeyWithAdjust (\k v -> runInputDisabledT $ f k v) (coerce dm0) (coerceEvent dm')
  -- traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = InputDisabledT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runInputDisabledT $ f k v) (coerce dm0) (coerceEvent dm')

withWSConnection ::
  (DomBuilder t m, PostBuild t m,
   MonadFix m, HasWebView m,
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
    (val,_, evList) <- runRWST (unWithWebSocketT wdgt) recvEv ()

    let
      -- sendEv = NE.toList <$> mergeList evList
      sendEv = evList
      conf = WebSocketConfig sendEv closeEv reconnect
    ws <- webSocket url conf
  return (val, ws)

codeToRun :: (DomBuilder t m) => WithWebSocketT t m ()
codeToRun = do
  ev <- button "hello"
  let ev2 = const Message <$> ev
  resp <- getResponse ev2
  let ev3 = showResp <$> resp
  return ()

myWidget ::
  (DomBuilder t m, PostBuild t m,
   MonadFix m, HasWebView m,
   PerformEvent t m, MonadIO m, MonadIO (Performable m),
   TriggerEvent t m)
  => m ()
myWidget = do
  text "Test WithWebsocket"
  (_,_) <- withWSConnection "ws://echo.websocket.org/" never False codeToRun
  return ()

main = mainWidget myWidget

data Resp = Resp
  deriving (Show, Generic)

data Message = Message
  deriving (Show, Generic)

instance ToJSON Message where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Message
instance ToJSON Resp where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Resp

showResp :: Resp -> String
showResp = show
