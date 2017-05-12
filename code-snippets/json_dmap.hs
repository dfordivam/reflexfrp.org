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

-- import Reflex.Dom
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
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer

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

type EncodeM = Writer ([ByteString], Map String (Some (Tag RealWorld )))

main = do

  (t1, m1) <- addVal Data.Dependent.Map.empty ("Voila" :: Text)
  (t2, m2) <- addVal m1 (2::Int)

  let dmapVals = Data.Dependent.Map.toList m2

      (_,(bs,mapId)) = runWriter $ mapM_ toString dmapVals

  -- print (t1,b1)
  -- print (t2,b2)

  -- let t1Str = show t1
  --     --t1' = (read t1Str) -- :: Tag RealWorld HasToJSON

  -- let v1 = fromBS b1 t1
  --     v2 = fromBS b1 t2
  --     v3 = fromBS b2 t1
  --     v4 = fromBS b2 t2

  --     val = Prelude.map isJust [v1,v2,v3,v4]
  -- print val
  return ()

addVal m v = do
  t <- newTag
  return (t, Data.Dependent.Map.insert t (HasToJSON v) m)

-- getTag :: DMap (Tag RealWorld) (HasFromJSON) -> String -> Maybe (Tag RealWorld v)
-- getTag dmap str =
--   where t
--   filter (\k -> show k == str) (keys dmap)

-- getRequestBS :: Event t (DMap (Tag (PrimState m)) HasToJSON) -> Event t [ByteString]
-- getRequestBS dmap = t <$> tup1
--   where
--     tup1 = (\d -> (keys d, d)) <$> dmap
--     tup d k = encode (k, d ! k)
--     t (k, d) -> map (tup d) k

toString ::
     DSum (Tag RealWorld) (HasToJSON)
  -> EncodeM ()
toString (t :=> HasToJSON a) = do
  tell ([BSL.toStrict $ encode (show t,a)], Map.singleton (show t) (This t))

fromBS ::
  (FromJSON (WebSocketResponseType v))
  => ByteString
  -> Tag RealWorld v
  -> Maybe (DMap (Tag RealWorld) (HasFromJSON))
fromBS bs t =
  case decodeStrict bs of
    Nothing -> Nothing
    Just v -> Just (Data.Dependent.Map.singleton t (HasFromJSON v))
