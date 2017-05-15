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

import Reflex.Dom hiding (Value, Error)
import Data.Aeson
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import WithWebSocket.Class
import Data.Unique.Tag
import Data.Dependent.Map
import Data.Dependent.Sum
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer

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
  HasToJSON :: ((FromJSON (WebSocketResponseType a)),ToJSON a) => a -> HasToJSON a
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
      evList = never :: Reflex t => (Event t [ByteString])
      recvEvMap = getReqs <$> sendEvMap

      getReqs dmap = mergeWith (<>) $
        getReq <$> (keys dmap)

      getReq ::
        (FromJSON (WebSocketResponseType v))
        => Tag m v
        -> Event t (DMap (Tag m) HasFromJSON)
      getReq t = fmapMaybe (fromBS t) (_webSocket_recv ws)

    (val, sendEvMap) <- runRequesterT wdgt recvEvMap
    let
      -- sendEv = NE.toList <$> mergeList evList
      sendEv = evList
      conf = WebSocketConfig sendEv closeEv reconnect
    ws <- webSocket url conf
  return (val, ws)

type TagMap = Map String (SomeWithFromJSON (Tag RealWorld))
type EncodeM = Writer ([ByteString], TagMap)

data SomeWithFromJSON tag where
        ThisWithFromJSON ::
            (FromJSON (WebSocketResponseType t)) =>
            !(tag t) -> SomeWithFromJSON tag

-- forall a. (ToJSON a, FromJSON (WebSocketResponseType a) ) =>
doEncode ::
  DSum (Tag RealWorld) (HasToJSON) -> EncodeM ()
doEncode (t :=> b@(HasToJSON a)) = do
  let
    f (t, HasToJSON a) =
      tell
        ( [BSL.toStrict $ encode (show t, a)]
        , Map.singleton (show t) (ThisWithFromJSON t))

    -- tAndA :: (Tag RealWorld _, _)
    tAndA = (t,b)
  f tAndA

-- make a tuple (Tag, Request)
getRequestBS
  :: (Reflex t)
  => Event t (DMap (Tag RealWorld) HasToJSON) -> Event t ([ByteString], TagMap)
getRequestBS dmapEv = f <$> dmapEv
  where
    f dmap = snd $ runWriter $ mapM_ doEncode (Data.Dependent.Map.toList dmap)

getResponseFromBS
  :: (Reflex t)
  => Dynamic t TagMap
  -> Event t ByteString
  -> Event t (DMap (Tag RealWorld) (HasFromJSON))
getResponseFromBS tagMap bs = fforMaybe inp decodeBSResponse
  where
    inp = attachPromptlyDyn tagMap bs

decodeBSResponse :: (TagMap, ByteString) -> Maybe (DMap (Tag RealWorld) (HasFromJSON))
decodeBSResponse (tagMap,bs) = join $ join $ g <$> taggy
  where
    -- Decode Tag first
    taggy =
      case decodeStrict bs of
        Nothing -> Nothing :: Maybe (String, Value)
        Just (str, rst) -> Just (str, rst)
    -- Given the tag, decode the rest of value
    g (str, rst) = f <$> t
      where
        t :: Maybe (SomeWithFromJSON (Tag RealWorld))
        t = Map.lookup str tagMap
        f (ThisWithFromJSON t') = decodeValue rst t'
    decodeValue
      :: (FromJSON (WebSocketResponseType v))
      => Value -> Tag RealWorld v -> Maybe (DMap (Tag RealWorld) (HasFromJSON))
    decodeValue bs t =
      case fromJSON bs of
        Error _ -> Nothing
        Success v -> Just (Data.Dependent.Map.singleton t (HasFromJSON v))
