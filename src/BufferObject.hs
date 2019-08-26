{-# LANGUAGE FlexibleContexts #-}

module BufferObject (bufferObject) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Functor ((<&>))
import           Data.StateVar (($=))
import           Graphics.Glow (Buffer, BufferData, BufferTarget, BufferUsage)
import qualified Graphics.Glow as Glow
import           Reflex

-- | Create a buffer object, fed by a 'Dynamic'. The input 'Dynamic'
-- is sampled on construction, and its updates cause the entire buffer
-- to be repopulated.
bufferObject
  :: ( BufferData a
     , MonadIO m
     , MonadIO (Performable m)
     , MonadSample t m
     , PerformEvent t m
     )
  => BufferTarget
  -> Dynamic t (BufferUsage, a)
  -> m (Buffer a)
bufferObject target dXs = do
  buf <- Glow.gen

  -- Populate the buffer
  initialXs <- sample $ current dXs
  liftIO . Glow.withBoundBufferAt target buf $
    Glow.bufferData target $= initialXs

  -- Update the buffer
  performEvent_ $ updated dXs <&> \xs -> liftIO $
    Glow.withBoundBufferAt target buf $ Glow.bufferData target $= xs

  pure buf
