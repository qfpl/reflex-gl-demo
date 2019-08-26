{-# OPTIONS_GHC -Wno-unused-matches #-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Uniform where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Dependent.Sum (DSum(..))
import qualified Data.Dependent.Sum as DSum
import           Data.Foldable (traverse_)
import           Data.Functor.Identity (Identity(..))
import           Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import           Data.GADT.Show.TH (deriveGShow)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.StateVar (($=))
import           Graphics.Glow (Mat4, Program)
import qualified Graphics.Glow as Glow

data UniformKey a where
  Mat4 :: UniformKey Mat4

$(deriveGEq ''UniformKey)
$(deriveGCompare ''UniformKey)
$(deriveGShow ''UniformKey)

instance DSum.EqTag UniformKey Identity where
  eqTagged Mat4 Mat4 = (==)

-- | Set of uniforms to apply to a shader program.
type Uniforms = Map String (DSum UniformKey Identity)

-- | Set the current program and apply a set of uniforms to it.
applyUniforms :: MonadIO m => Program -> Uniforms -> m ()
applyUniforms prog uniforms = do
  Glow.currentProgram $= prog
  traverse_ (uncurry applyUniform) $ Map.toList uniforms
  where
    applyUniform name uniform = do
      loc <- Glow.uniformLocation prog name
      unless (loc == -1) $ case uniform of
        Mat4 :=> Identity m -> Glow.uniformMat4 loc m
