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
import GHC.Generics
import Data.Unique.Tag

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

main = do
  let addVal m v = do
        t <- newTag
        return (t, Data.Dependent.Map.insert t (HasToJSON v) m)

  (t1, m1) <- addVal Data.Dependent.Map.empty ("Voila" :: Text)
  (t2, m2) <- addVal m1 (2::Int)

  let dmapVals = Data.Dependent.Map.toList m2

      b1 = Prelude.map toString dmapVals
  print b1
  return ()

toString :: DSum t (HasToJSON) -> ByteString
toString (_ :=> HasToJSON a) = BSL.toStrict $ encode a
