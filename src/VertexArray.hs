{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module VertexArray
  ( VertexArrayConfig
  , VertexArrayUpdates
  , SomeBuffer(..)
  , vertexArray
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Functor ((<&>), void)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.Misc (diffMap)
import           Data.StateVar (($=))
import           Data.Typeable
import           Graphics.Glow (AttributeLocation)
import qualified Graphics.Glow as Glow
import           Reflex

data SomeBuffer where
  SomeBuffer :: (Eq a, Typeable a) => Glow.Buffer a -> SomeBuffer

instance Eq SomeBuffer where
  (SomeBuffer (a :: Glow.Buffer a)) == (SomeBuffer (b :: Glow.Buffer b))
    = maybe False (\Refl -> a == b) $ eqT @a @b

type VertexArrayConfig = Map AttributeLocation (Maybe SomeBuffer, Glow.Layout)
type VertexArrayUpdates = Map AttributeLocation
  (Maybe (Maybe SomeBuffer, Glow.Layout))

vertexArray
  :: (MonadIO m, MonadIO (Performable m), MonadSample t m, PerformEvent t m)
  => Dynamic t VertexArrayConfig
  -> m Glow.VertexArray
vertexArray dConfig = do
  let eDiffs = uncurry diffMap <$> attach (current dConfig) (updated dConfig)
  vao <- Glow.gen

  -- Set up the VAO
  initialState <- sample $ current dConfig
  liftIO . Glow.withVertexArray vao . applyUpdates $ Just <$> initialState

  -- Update the VAO
  performEvent_ $ eDiffs <&> \diff -> liftIO $
    Glow.withVertexArray vao $ applyUpdates diff

  pure vao

applyUpdates :: VertexArrayUpdates -> IO ()
applyUpdates updates = void . flip Map.traverseWithKey updates $ \loc mCfg ->
  case mCfg of
    Nothing -> Glow.setVertexAttribute loc $= Nothing
    Just (mBuf, layout) ->
      let
        -- If we're reading from the buffer, bind it first
        bindBufferFunc = case mBuf of
          Nothing -> id
          Just (SomeBuffer buf) ->
            Glow.withBoundBufferAt Glow.ArrayBufferTarget buf
      in
        bindBufferFunc $ Glow.setVertexAttribute loc $= Just layout
