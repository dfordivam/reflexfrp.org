-- Merge Events from multiple sources and then split them

-- Simple approach
-- If we have a single web socket connection and multiple widgets which want to communicate
-- over it, then we need to merge and fan the events.
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Data.Text as T
import Data.Monoid

-- The two widgets need to send this
data A = A Int
data B = B Text

-- And get these response
data AResp = AResp Bool
data BResp = BResp Int

-- Simple
data Query =
    QueryA A
  | QueryB B

data Response =
    ResponseA AResp
  | ResponseB BResp

widgetAReq :: Event t A
widgetBReq :: Event t B

-- merge
wsSendEv = leftmost [QueryA <$> widgetAReq, QueryB <$> widgetBReq]

decodedWsResp :: Event t Response

-- fan
widgetAResp = fforMaybe decodedWsResp
                (\case
                  (ResponseA a) -> Just a
                  _ -> Nothing)

widgetBResp = fforMaybe decodedWsResp
                (\case
                  (ResponseB b) -> Just b
                  _ -> Nothing)

-- Ideal interface for getting the websocket response
class (Widget m) => WithWebSocket m where
  type Response a
  getResponse (ToJSON req, FromJSON (Response req))
    :: Event t req -> m (Event t (Response req))

getResponse req = do
  -- Add Event t ByteString for sendEv
  tell (encode <$> req)
  -- Provide the response
  ev <- ask
  fforMaybe decode ev
-- merge implementation is simple
-- Just call encode and get bytestring

-- For fanning the decode can be either duplicated

type Response A = AResp
type Response B = BResp

-- Run code which shares single WS Connection
withWSConnection ::
  (WithWebSocket mw, Widget m)
     Text -- URL
  -> Event t Text -- close event
  -> Bool -- reconnect
  -> mw (a)
  -> m (a, WebSocket)
withWSConnection url closeEv reconnect wdgt = do
  rec
    let
      recvEv = _webSocket_recv ws
        f r s = do
          (val, evList) <- listen  wdgt
          return (val,(), evList)
    (val,_, evList) <- runRWST f recvEv ()

    let
      sendEv = NE.toList <$> mergeList evList
      conf = WebSocketConfig sendEv closeEv reconnect
    ws <- webSocket url conf
  return val
