{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module WithWebSocket.Class where

import Reflex.Dom
import Data.Aeson
import Control.Monad.Trans
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive

type family WebSocketResponseType a :: *
class
     (DomBuilder t m, PostBuild t m, PrimMonad m,
     MonadFix m, HasWebView m, MonadHold t m,
     PerformEvent t m, MonadIO m, MonadIO (Performable m),
     TriggerEvent t m)
  => WithWebSocket t m | m -> t where
  getWebSocketResponse ::
    (ToJSON a, FromJSON (WebSocketResponseType a),
     WithWebSocket t m)
     => Event t a -> m (Event t (WebSocketResponseType a))
