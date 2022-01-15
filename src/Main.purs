module Main where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (logShow, log)
import Foreign.Object (values, lookup)
import Screeps.Game (getGameGlobal, creeps, spawns)
import Screeps.Memory (get, getMemoryGlobal, set)
import Screeps.Role.Harvester as H
import Screeps.Role.Upgrader as U
import Screeps.Role.Common (CommonError(..))

data ScreepsError =
  HarvesterErr H.HarvesterError
  | UpgraderErr U.UpgraderError
derive instance genericScreepsError :: Generic ScreepsError _
instance showScreepsError :: Show ScreepsError where show = genericShow

creep_counter :: String
creep_counter = "creep_counter"

main :: Effect Unit
main = do
  game <- getGameGlobal
  memory <- getMemoryGlobal
  let spawn1 = lookup "Spawn1" $ spawns game
      myCreeps = creeps game
  case spawn1 of
    Nothing -> log "Can't find Spawn1"
    Just s1 -> do
      n <- get memory creep_counter
      spawnHarvesterResult <- H.spawn (H.role <> "0") s1 (values myCreeps)
      case spawnHarvesterResult of
        Left (H.HarvesterCommonErr (CreepWithNameExistsErr _)) -> incrCreepCounter memory n
        Right _ -> incrCreepCounter memory n
        Left _ -> pure unit
      harvesterResults <- map (lmap HarvesterErr) <$> traverse (H.run s1) (values myCreeps)
      n' <- get memory creep_counter
      spawnUpgraderResult <- U.spawn (U.role <> (either (const "0") show n')) s1 (values myCreeps)
      case spawnUpgraderResult of
        Left (U.UpgraderCommonErr (CreepWithNameExistsErr _)) -> incrCreepCounter memory n'
        Right _ -> incrCreepCounter memory n'
        Left _ -> pure unit
      upgraderResults <- map (lmap UpgraderErr) <$> traverse (U.run s1) (values myCreeps)
      logShow spawnHarvesterResult
      logShow harvesterResults
      logShow spawnUpgraderResult
      logShow upgraderResults
  where
    incrCreepCounter mem n = set mem creep_counter (either (const 0) (_ + 1) n)
