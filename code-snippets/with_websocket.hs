{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Reflex.Dom
import Data.Aeson
import Data.ByteString.Lazy
import Control.Monad.RWS.Class

-- Ideal interface for getting the websocket response
class (DomBuilder t m,
       MonadRWS (Event t ByteString) (Event t [ByteString]) () m)
  => WithWebSocket t m where
  getResponse :: (ToJSON req, FromJSON resp)
    => Event t req -> m (Event t resp)

enc :: (ToJSON a, Functor f) => f a -> f [ByteString]
enc mes = (:[]) <$> encode <$> mes

instance (DomBuilder t m,
       MonadRWS (Event t ByteString) (Event t [ByteString]) () m)
  => WithWebSocket t m where
  getResponse req = do
    -- Add Event t ByteString for sendEv
    tell ((:[]) <$> encode <$> req)
    -- Provide the response
    ev <- ask
    return $ fforMaybe ev decode
  -- merge implementation is simple
  -- Just call encode and get bytestring
main = undefined
