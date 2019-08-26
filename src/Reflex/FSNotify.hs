{-# LANGUAGE GADTs           #-}

module Reflex.FSNotify (watchDir) where

import           Reflex
import qualified System.FSNotify as FS

watchDir
  :: TriggerEvent t m
  => FS.WatchManager -> FilePath -> m (Event t FS.Event)
watchDir manager path = newEventWithLazyTriggerWithOnComplete $ \fire ->
  FS.watchDir manager path (\_ -> True) (\fsEvent -> fire fsEvent (pure ()))
