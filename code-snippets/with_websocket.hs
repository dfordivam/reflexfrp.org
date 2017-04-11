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
import Data.ByteString.Lazy
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

enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
enc mes = (:[]) <$> encode <$> mes

instance (DomBuilder t m) => WithWebSocket t (WithWebSocketT t m) where
  getResponse req = do
    -- (WithWebSocketT rwst)
    -- Add Event t ByteString for sendEv
    tell ((:[]) <$> encode <$> req)
    -- Provide the response
    ev <- ask
    return $ fforMaybe ev decode
  -- merge implementation is simple
  -- Just call encode and get bytestring

-- type WithWebSocket t m = MonadRWS (Event t ByteString) (Event t [ByteString]) () m
 -- (RWST (Event t ByteString) (Event t [ByteString]) () m)

instance (Reflex t) => MonadTransControl (WithWebSocketT t) where
  type StT (WithWebSocketT t) a = StT (RWST (Event t ByteString) (Event t [ByteString]) ()) a
  liftWith = defaultLiftWith WithWebSocketT unWithWebSocketT
  restoreT = defaultRestoreT WithWebSocketT

newtype WithWebSocketT t m a =
  WithWebSocketT
  { unWithWebSocketT :: RWST (Event t ByteString) (Event t [ByteString]) () m a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans)

instance (Reflex t, Monad m) =>
  MonadReader (Event t ByteString) (WithWebSocketT t m)
instance (Reflex t, Monad m) =>
  MonadWriter (Event t [ByteString]) (WithWebSocketT t m)
instance DomBuilder t m => DomBuilder t (WithWebSocketT t m) where
  type DomBuilderSpace (WithWebSocketT t m) = DomBuilderSpace m
  -- {-# INLINABLE textNode #-}
  -- textNode cfg = lift $ textNode cfg
  element t cfg child = do
    (ret, w) <- liftWith $ \run -> do
      let myEl r = do
            (a,_,w) <- r
            ret <- element t (liftElementConfig cfg) (pure a)
            return (ret, w)
      myEl (run child)
    tell w
    return ret

      -- let myRun b = do
      --       (a,_, w) <- run b
      --       return a
      -- element t (liftElementConfig cfg) $ myRun child
  {-# INLINABLE element #-}
  -- {-# INLINABLE element #-}
  -- element t cfg child = lift $ element t cfg $ run
  --   --liftWith $ \run -> element t cfg $ run child
  -- {-# INLINABLE inputElement #-}
  -- inputElement cfg = lift $ inputElement cfg
  -- {-# INLINABLE textAreaElement #-}
  -- textAreaElement cfg = lift $ textAreaElement cfg
  {-# INLINABLE selectElement #-}
  selectElement cfg child = do
    (ret, w) <- liftWith $ \run -> do
      let myEl r = do
            (a,_,w) <- r
            ret <- selectElement cfg' (pure a)
            return (ret, w)
          cfg' = cfg
                  { _selectElementConfig_elementConfig =
                    liftElementConfig $ _selectElementConfig_elementConfig cfg
                  }
      myEl (run child)
    tell w
    return ret

instance (MonadAdjust t m) => MonadAdjust t (WithWebSocketT t m)
  -- inputElement = lift inputElement
  -- textAreaElement = lift textAreaElement
  -- selectElement = lift selectElement


-- getResponse :: (ToJSON req, FromJSON resp, Reflex t, WithWebSocket t m)
--   => Event t req -> m (Event t resp)
-- getResponse req = do
--   -- Add Event t ByteString for sendEv
--   tell ((:[]) <$> encode <$> req)
--   -- Provide the response
--   ev <- ask
--   return $ fforMaybe ev decode

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

-- codeToRun :: (WithWebSocket t m) => m ()
-- codeToRun = do
--   ev <- button "hello"
--   let ev2 = const Message <$> ev
--   resp <- getResponse ev2
--   let ev3 = showResp <$> resp
--   return ()

-- myWidget :: (DomBuilder t m) => m ()
-- myWidget = do
--   (_,_) <- withWSConnection "url" never False codeToRun
--   return ()
-- Run code which shares single WS Connection
withWSConnection ::
  (DomBuilder t m, WithWebSocket t mw)
  => Text -- URL
  -> Event t (Word, Text) -- close event
  -> Bool -- reconnect
  -> mw a
  -> m (a, WebSocket t)
withWSConnection url closeEv reconnect wdgt = undefined
  -- rec
  --   let
  --     recvEv = _webSocket_recv ws
  --     f r s = do
  --         (val, evList) <- listen  wdgt
  --         return (val,(), evList)
  --   (val,_, evList) <- runRWST (RWST f) recvEv ()

  --   let
  --     -- sendEv = NE.toList <$> mergeList evList
  --     sendEv = evList
  --     conf = WebSocketConfig sendEv closeEv reconnect
  --   ws <- webSocket url conf
  -- return (val, ws)
