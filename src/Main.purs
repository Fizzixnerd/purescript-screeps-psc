module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Screeps
import Screeps.Spawn
import Screeps.Game
import Foreign.Object (keys)
import Data.Traversable (traverse)

main :: Effect Unit
main = do
  game <- getGameGlobal
  let mySpawns = spawns game
  void $ traverse log (keys mySpawns)

