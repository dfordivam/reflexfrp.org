{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text
import Data.List.NonEmpty as NE

import GHC.Generics

-- Ideal interface for getting the websocket response
-- class (DomBuilder t m,
--        MonadRWS (Event t ByteString) (Event t [ByteString]) () m)
--   => WithWebSocket t m where
--   getResponse :: (ToJSON req, FromJSON resp)
--     => Event t req -> m (Event t resp)

-- newtype WithWebSocket t m =
--   WithWebSocket
--   { withWS :: m a -> RWST (Event t ByteString) (Event t [ByteString]) () m a}

enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
enc mes = (:[]) <$> encode <$> mes

-- instance (DomBuilder t m,
--        MonadRWS (Event t ByteString) (Event t [ByteString]) () m)
--   => WithWebSocket t m where
--   getResponse req = do
--     -- Add Event t ByteString for sendEv
--     tell ((:[]) <$> encode <$> req)
--     -- Provide the response
--     ev <- ask
--     return $ fforMaybe ev decode
  -- merge implementation is simple
  -- Just call encode and get bytestring
main = undefined

type WithWebSocket t m = MonadRWS (Event t ByteString) (Event t [ByteString]) () m

getResponse :: (ToJSON req, FromJSON resp, Reflex t, WithWebSocket t m)
  => Event t req -> m (Event t resp)
getResponse req = do
  -- Add Event t ByteString for sendEv
  tell ((:[]) <$> encode <$> req)
  -- Provide the response
  ev <- ask
  return $ fforMaybe ev decode

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

codeToRun :: (DomBuilder t m, WithWebSocket t m) => m ()
codeToRun = do
  ev <- button "hello"
  let ev2 = const Message <$> ev
  resp <- getResponse ev2
  let ev3 = showResp <$> resp
  return ()

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
