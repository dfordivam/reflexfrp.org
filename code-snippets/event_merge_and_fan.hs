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
