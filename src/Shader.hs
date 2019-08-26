{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Shader (compile, link, watchShaderProgram) where

import           Prelude hiding (filter)

import           Control.Applicative (liftA2)
import           Control.Lens (pattern Strict, _1, view)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Foldable (traverse_)
import           Data.Functor (($>), (<&>))
import           Data.StateVar (($=))
import           Data.Witherable (catMaybes, filter)
import           Graphics.Glow (Program, Shader, ShaderType)
import qualified Graphics.Glow as Glow
import           Path
import           Reflex
import           Reflex.FSNotify
import qualified System.FSNotify as FS

-- | Create and compile a shader each time the event fires. When a
-- shader is successfully compiled, the previous shader is marked for
-- deletion and a new one is created.
compile
  :: (MonadHold t m, MonadIO m, PerformEvent t m, MonadIO (Performable m))
  => ShaderType
  -> Event t ByteString
  -> m (Event t (Either ByteString Shader))
compile sType eCode = do
  eeShader <- performEvent $ eCode <&> \c -> do
    shader <- Glow.createShader sType
    Glow.shaderSource shader $= Strict c
    Glow.compileShader shader
    Glow.compileStatus shader >>= \case
      True -> pure $ Right shader
      False -> do
        err <- Glow.shaderInfoLog shader
        Glow.delete shader
        pure $ Left err

  dShader <- holdDyn Nothing . fmap Just . filterRight $ eeShader

  -- When we get a new shader, delete the old one
  performEvent_ $ traverse_ Glow.delete <$> current dShader <@ updated dShader

  pure eeShader

-- | Link a shader program after each event has fired at least
-- once. When a program is successfully linked, the old one is
-- deleted.
link
  :: ( MonadHold t m
     , MonadIO (Performable m)
     , PerformEvent t m
     )
  => Event t Shader -- ^ Vertex Shader
  -> Event t Shader -- ^ Fragment Shader
  -> m (Event t (Either ByteString Program))
link eVs eFs = do
  dVs <- holdDyn Nothing $ Just <$> eVs
  dFs <- holdDyn Nothing $ Just <$> eFs

  let
    dmShaders = zipDynWith (liftA2 (,)) dVs dFs
    eShaders = catMaybes $ updated dmShaders

  eeProgram <- performEvent $ eShaders <&> \(vs, fs) -> do
    program <- Glow.gen
    Glow.attachShader program vs
    Glow.attachShader program fs
    Glow.linkProgram program
    Glow.linkStatus program >>= \case
      True -> pure $ Right program
      False -> do
        err <- Glow.programInfoLog program
        Glow.delete program
        pure $ Left err

  let eProgram = filterRight eeProgram
  dProgram <- holdDyn Nothing $ Just <$> eProgram

  -- When we get a new program, delete the old one
  performEvent_ $ traverse_ Glow.delete <$> current dProgram <@ updated dProgram

  pure eeProgram

watchShaderProgram
  :: ( MonadHold t m
     , MonadIO m
     , MonadIO (Performable m)
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     )
  => FS.WatchManager
  -> Path Abs Dir -- ^ Directory name
  -> Path Rel File -- ^ Vertex shader name (relative to dir)
  -> Path Rel File -- ^ Fragment shader name (relative to dir)
  -> m (Event t Program)
watchShaderProgram manager dir vsFileRel fsFileRel = do
  ePostBuild <- getPostBuild

  let
    vsFileAbs = toFilePath $ dir </> vsFileRel
    fsFileAbs = toFilePath $ dir </> fsFileRel

  -- Read the initial sources
  vsInitial <- liftIO $ B.readFile vsFileAbs
  fsInitial <- liftIO $ B.readFile fsFileAbs

  -- Watch the dir for changes to the files we're interested in. In
  -- this demo program, we only care about one of the constructors of
  -- a 'FS.Event'. This means we can get away with
  -- 'mapMaybe'.
  --
  -- Exercise: Learn how to use 'fan' to efficiently split an 'Event t
  -- FS.Event' into four different 'Event's.
  let
    onlyModifications (FS.Modified path mtime isDir) = Just (path, mtime, isDir)
    onlyModifications _ = Nothing
  eDirChanges <- fmap (view _1) . mapMaybe onlyModifications
    <$> watchDir manager (toFilePath dir)

  let
    eVsChanges = filter (vsFileAbs ==) eDirChanges
    eFsChanges = filter (fsFileAbs ==) eDirChanges
  eVsUpdates <- performEvent $ liftIO . B.readFile <$> eVsChanges
  eFsUpdates <- performEvent $ liftIO . B.readFile <$> eFsChanges

  -- Tie the shader sources together and compile
  eeVs <- compile Glow.VertexShader $ leftmost
    [ eVsUpdates
    , ePostBuild $> vsInitial
    ]
  eeFs <- compile Glow.FragmentShader $ leftmost
    [ eFsUpdates
    , ePostBuild $> fsInitial
    ]

  -- Report compile errors
  let
    (eVsError, eVs) = fanEither eeVs
    (eFsError, eFs) = fanEither eeFs
  performEvent_ $ eVsError <&> \err -> liftIO . B.putStrLn $
    "Vertex Shader Compile Error: " <> err
  performEvent_ $ eFsError <&> \err -> liftIO . B.putStrLn $
    "Fragment Shader Compile Error: " <> err

  -- Link and report link errors
  eeProg <- link eVs eFs
  let (eLinkError, eProg) = fanEither eeProg
  performEvent_ $ eLinkError <&> \err -> liftIO . B.putStrLn $
    "Shader Link Error: " <> err

  pure eProg
