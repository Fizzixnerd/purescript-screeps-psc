module Screeps.Monad where

import Effect (Effect)
import Data.Either (Either)

type ScreepsM' m e a = m (Either e a)

type ScreepsM e a = ScreepsM' Effect e a
