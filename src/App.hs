{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module App where

import           Prelude hiding (filter)

import           BufferObject
import           Camera
import           Shader
import           Uniform
import           VertexArray

import           Control.Lens.Operators
import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Ref (modifyRef, newRef, readRef)
import           Data.Bits ((.|.))
import           Data.Dependent.Sum ((==>))
import           Data.Foldable (for_, traverse_)
import           Data.Functor (($>))
import           Data.Functor.Misc (Const2(..))
import           Data.List (genericLength)
import           Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.StateVar (($~), ($=))
import           Data.Traversable (for)
import           Foreign.Ptr (nullPtr)
import           Graphics.GL.Core45
import           Graphics.GL.Types (GLenum, GLint, GLsizei)
import qualified Graphics.Glow as Glow
import           Graphics.Glow hiding (Program)
import qualified Graphics.UI.GLFW as GLFW
import           Linear (V2(..), V3(..), _xy)
import           Path (parseAbsDir, relfile)
import           Reflex
import           Reflex.Host.Basic
import           System.Directory (canonicalizePath)
import           System.Exit (exitFailure)
import qualified System.FSNotify as FS

main :: IO ()
main = do
  window <- setupGL
  FS.withManager $ \manager -> basicHostWithQuit (guest window manager)
  GLFW.terminate

guest
  :: forall t m . BasicGuestConstraints t m
  => GLFW.Window
  -> FS.WatchManager
  -> BasicGuest t m (Event t ())
guest window manager = do
  (quitE, quitF) <- newTriggerEvent

  eTick <- tickLossyFromPostBuildTime $ 1 / 60
  ePumpedInputs <- performEventAsync $ eTick $> pumpInput window quitF

  -- Construct `bInputs`, a `Behavior (Map GLFW.Key ())` that has a value
  -- at each key only for the keys currently being pressed.
  --
  -- This slightly strange structure lets us use `fanMap` later to
  -- efficiently split out events for each key press.
  let
    toChange :: (GLFW.Key, GLFW.KeyState) -> Map GLFW.Key () -> Map GLFW.Key ()
    toChange (k, GLFW.KeyState'Pressed) = Map.insert k ()
    toChange (k, GLFW.KeyState'Released) = Map.delete k
    toChange (_, GLFW.KeyState'Repeating) = id

    eInputChanges :: Event t (Map GLFW.Key () -> Map GLFW.Key ())
    eInputChanges = foldr1 (.) . fmap toChange <$> ePumpedInputs
  bInputs <- current <$> foldDyn ($) Map.empty eInputChanges

  -- We sample `bInputs` on each tick, and then use `fanMap` to get
  -- events for each key that fire once per tick while they are held
  -- down.
  let
    eInputs = bInputs <@ eTick
    eInputSelector = fanMap eInputs

    key k = eInputSelector `select` Const2 k

    eForward = key GLFW.Key'W
    eStrafeLeft = key GLFW.Key'A
    eBack = key GLFW.Key'S
    eStrafeRight = key GLFW.Key'D
    eLeft = key GLFW.Key'Left
    eRight = key GLFW.Key'Right
    eUp = key GLFW.Key'Up
    eDown = key GLFW.Key'Down

  -- Set up the camera. The `rec` block lets use the camera's position
  -- and facing to determine the movement vectors, which we feed into
  -- the camera config to geet the camera position. This is possible
  -- due to laziness and `MonadFix` magic.
  rec
    let
      moveSize = 0.1
      -- `mkDynPure` is a Template Haskell QuasiQuoter that lets us
      -- write complex expressions involving `Dynamic`s without having
      -- to make a tangle of `Applicative` operators.
      moveVec :: Float -> Dynamic t Vec2
      moveVec offset = [mkDynPure|
        moveSize * V2
          (cos ($(cam ^. cdYaw) + offset))
          (sin ($(cam ^. cdYaw) + offset))
      |]

      eMoves = mergeWith (.) $ fmap (_xy +~) <$>
        [ current (moveVec 0) <@ eForward
        , current (moveVec pi) <@ eBack
        , current (moveVec $ pi / 2) <@ eStrafeLeft
        , current (moveVec $ 3 * pi / 2) <@ eStrafeRight
        ]
      ePitches = mergeWith (.)
        [ eUp $> subtract 0.05
        , eDown $> (+ 0.05)
        ]
      eYaws = mergeWith (.)
        [ eLeft $> (+ 0.05)
        , eRight $> subtract 0.05
        ]

      camconf = CameraConfig (V3 (-20) 0 10) 0 0 ePitches eYaws eMoves
    cam <- camera camconf

  let
    haskellLogo :: [[Vec3]]
    haskellLogo =
      [ [ V3 0 0 0, V3 (-5) 5 0, V3 (-4) 5 0, V3 1 0 0 ]
      , [ V3 0 0 0, V3 1 0 0, V3 (-4) (-5) 0, V3 (-5) (-5) 0 ]
      , [ V3 7 (-5) 0, V3 (-3) 5 0, V3 (-2) 5 0, V3 8 (-5) 0 ]
      , [ V3 2 0 0, V3 3 0 0, V3 (-2) (-5) 0, V3 (-3) (-5) 0 ]
      , [ V3 2.5 1 0, V3 8 1 0, V3 8 0.5 0, V3 3 0.5 0 ]
      , [ V3 3.5 0 0, V3 8 0 0, V3 8 (-0.5) 0, V3 4 (-0.5) 0 ]
      ]

  -- Construct a vertex buffer for each row of points.
  vertexBuffers <- for haskellLogo $ \points -> do
    vbo <- bufferObject ArrayBufferTarget $ constDyn (StaticDraw, points)
    pure (vbo, genericLength points)

  -- Construct the VAOs, which tell OpenGL how to use each VBO and
  -- use their contents.
  vaos <- for vertexBuffers $ \(vbo, _) -> vertexArray . constDyn $ Map.fromList
    [ (0, (Just $ SomeBuffer vbo, Layout 3 GL_FLOAT False 0 nullPtr)) ]

  -- Build and link our shader program, and store it in `bmProg`.
  dataDir <- liftIO $ canonicalizePath "dat" >>= parseAbsDir
  eProg <- watchShaderProgram manager
    dataDir [relfile|vertex.glsl|] [relfile|fragment.glsl|]
  bmProg <- hold Nothing $ Just <$> eProg

  -- Build `bDraws`, our list of things to draw each frame. It is
  -- sampled each time `eTick` fires and redrawn by `draw`.
  let
    bDraws :: Behavior t [Draw]
    bDraws = do
      matrix <- current $ cam ^. cdProjectionMatrix
      mProg <- bmProg
      -- Draw from each VAO, using the length of each VBO.
      let drawWithProgram p = zip vaos vertexBuffers <&> \(vao, (_, size)) ->
            Draw
              p
              vao
              (Map.fromList [("proj", Mat4 ==> matrix)])
              GL_TRIANGLE_FAN
              0
              size

      pure $ foldMap drawWithProgram mProg
  draw window $ bDraws <@ eTick

  pure quitE

data Draw = Draw Glow.Program Glow.VertexArray Uniforms GLenum GLint GLsizei

setupGL :: IO GLFW.Window
setupGL = do
  ok <- GLFW.init
  unless ok $ putStrLn "GLFW init failed" *> exitFailure

  traverse_ GLFW.windowHint
    ([ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
     , GLFW.WindowHint'ContextVersionMajor 4
     , GLFW.WindowHint'ContextVersionMinor 1
     , GLFW.WindowHint'OpenGLForwardCompat True
     , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
     ] :: [GLFW.WindowHint])

  win <- GLFW.createWindow 800 600 "Hello Compose!" Nothing Nothing >>= \case
    Nothing ->
      putStrLn "GLFW createWindow failed" *> GLFW.terminate *> exitFailure
    Just win -> GLFW.makeContextCurrent (Just win) $> win

  glEnable GL_DEPTH_TEST
  stateBits $~ depthFunc .~ DepthFuncLEqual

  pure win

-- | Poll OpenGL for events, and fire the relevant reflex event
-- triggers.
pumpInput
  :: MonadIO m
  => GLFW.Window
  -> (() -> IO ()) -- ^ Trigger quit event
  -> (NonEmpty (GLFW.Key, GLFW.KeyState) -> IO ()) -- ^ Trigger input event
  -> m ()
pumpInput window triggerQuit triggerInput = liftIO $ do
  shouldQuit <- GLFW.windowShouldClose window
  if shouldQuit
    then triggerQuit ()
    else liftIO $ newRef [] >>= \keysR -> do
      let keyCallback _ key _ state _ =
            modifyRef keysR ((key, state):)
      GLFW.setKeyCallback window . Just $ keyCallback
      GLFW.pollEvents
      readRef keysR >>= \keys ->
        traverse_ triggerInput (nonEmpty $ reverse keys)

-- | Redraw the screen with a new '[Draw]' each time it fires.
draw
  :: (MonadIO m, PerformEvent t m, MonadIO (Performable m), Reflex t)
  => GLFW.Window
  -> Event t [Draw]
  -> m ()
draw win eDraws = performEvent_ $ eDraws <&> \draws ->
  liftIO $ do
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
    for_ draws $ \(Draw prog vao uni prim start n) -> do
      currentProgram $= prog
      applyUniforms prog uni
      withVertexArray vao $ glDrawArrays prim start n
    GLFW.swapBuffers win
