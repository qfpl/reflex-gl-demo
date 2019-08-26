{-# LANGUAGE GADTs               #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module Camera where

import Control.Lens (over, view)
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Fix (MonadFix)
import Data.Clamped (clamped, mkClamped)
import Data.Wraparound (mkWraparound, wraparound)
import Graphics.Glow (Mat4, Vec3)
import Linear.Matrix ((!*!))
import Linear.Projection (lookAt, perspective)
import Linear.Quaternion (axisAngle, rotate)
import Linear.V3 (V3(..), cross)
import Reflex

data CameraConfig t = CameraConfig
  { _ccInitialEye :: Vec3
  , _ccInitialPitch :: Float
  , _ccInitialYaw :: Float
  , _ccePitch :: Event t (Float -> Float)
  , _cceYaw :: Event t (Float -> Float)
  , _cceTranslate :: Event t (Vec3 -> Vec3)
  }

$(makeLenses ''CameraConfig)

data Camera t = Camera
  { _cdEye :: Dynamic t Vec3
  , _cdFocus :: Dynamic t Vec3
  , _cdPitch :: Dynamic t Float
  , _cdYaw :: Dynamic t Float
  , _cdProjectionMatrix :: Dynamic t Mat4
  }

$(makeLenses ''Camera)

camera
  :: forall t m . (MonadFix m, MonadHold t m, Reflex t)
  => CameraConfig t
  -> m (Camera t)
camera cc = do
  dEye <- foldDyn ($) (_ccInitialEye cc) $ _cceTranslate cc

  dPitch <- foldDyn ($)
    (mkClamped (negate (pi / 2 - 0.01)) (pi / 2 - 0.01) $ _ccInitialPitch cc)
    (over clamped <$> _ccePitch cc)

  dYaw <- foldDyn ($) (mkWraparound 0 (2 * pi) $ _ccInitialYaw cc) $
    over wraparound <$> _cceYaw cc

  let
    up = V3 0 0 1 :: Vec3
    dYawQ = axisAngle up . view wraparound <$> dYaw

    dPitchAxis = dYaw <&>
      cross up . (\(view wraparound -> yaw) -> V3 (cos yaw) (sin yaw) 0)
    dPitchQ = axisAngle <$> dPitchAxis <*> (view clamped <$> dPitch)

    dFocus :: Dynamic t Vec3
    dFocus = [mkDynPure| $dEye + rotate ($dPitchQ * $dYawQ) (V3 1 0 0) |]

    dViewMatrix :: Dynamic t Mat4
    dViewMatrix = lookAt <$> dEye <*> dFocus <*> pure up

    perspectiveMatrix = perspective (pi/3) (640/480) 1 100

  pure $ Camera
    dEye
    dFocus
    (view clamped <$> dPitch)
    (view wraparound <$> dYaw)
    ((perspectiveMatrix !*!) <$> dViewMatrix)
