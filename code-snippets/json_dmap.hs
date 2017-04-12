{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

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
import Data.Dependent.Map
import Data.Dependent.Sum
import GHC.Generics
import Data.Unique.Tag
import Control.Monad.Primitive
import Data.Maybe

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

type family WebSocketResponseType a :: *

data HasToJSON a where
  HasToJSON :: (ToJSON a) => a -> HasToJSON a
data HasFromJSON a where
  HasFromJSON :: (FromJSON (WebSocketResponseType a)) => (WebSocketResponseType a) -> HasFromJSON a

type instance WebSocketResponseType Int = Text
type instance WebSocketResponseType Text = Bool

main = do
  let addVal m v = do
        t <- newTag
        return (t, Data.Dependent.Map.insert t (HasToJSON v) m)

  (t1, m1) <- addVal Data.Dependent.Map.empty ("Voila" :: Text)
  (t2, m2) <- addVal m1 (2::Int)

  let dmapVals = Data.Dependent.Map.toList m2

      [b1,b2] = Prelude.map toString dmapVals
  print b1
  print b2


  let v1 = fromBS b1 t1
      v2 = fromBS b1 t2
      v3 = fromBS b2 t1
      v4 = fromBS b2 t2

      val = Prelude.map isJust [v1,v2,v3,v4]
  print val
  return ()

toString :: DSum t (HasToJSON) -> ByteString
toString (_ :=> HasToJSON a) = BSL.toStrict $ encode a

fromBS ::
  (FromJSON (WebSocketResponseType v))
  => ByteString
  -> Tag m v
  -> Maybe (DMap (Tag m) (HasFromJSON))
fromBS bs t =
  case decodeStrict bs of
    Nothing -> Nothing
    Just v -> Just (Data.Dependent.Map.singleton t (HasFromJSON v))
