module Screeps.Role.Upgrader where

import Prelude

import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe, Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Screeps (Creep, Spawn, part_carry, part_move, part_work, Controller)
import Screeps.Constants (resource_energy)
import Screeps.Controller (level, ticksToDowngrade)
import Screeps.Creep (amtCarrying, freeCapacity, upgradeController)
import Screeps.Monad (ScreepsM)
import Screeps.Role.Common (CommonError(..), Role(..), gatherEnergyThen, isNear, maxLevel, mkCount, mkRun, mkSpawn, moveToThen, returnEnergyToBase, throwM)
import Screeps.Room (controller)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (inRangeTo)
import Screeps.Spawn (SpawnOptions, spawnOpts)
import Screeps.Types (BodyPartType, TargetPosition(..))

data UpgraderError =
  NotEnoughEnergyToSpawnCreepErr
  | CreepWithNameExistsErr
  | SpawnBusyErr
  | SpawnEnergyFullErr
  | UpgraderCommonErr CommonError

derive instance genericUpgraderError :: Generic UpgraderError _
instance showUpgraderError :: Show UpgraderError where show = genericShow

type UpgraderMemory e =
  { role :: Role | e }

parts :: Array BodyPartType
parts = [part_carry, part_move, part_work]

role :: Role
role = Upgrader

count :: Effect Int
count = mkCount role

memory :: UpgraderMemory ()
memory = { role: role }

opts :: SpawnOptions (UpgraderMemory ())
opts = spawnOpts { memory = Just memory }

spawn :: String -> Spawn -> Array Creep -> ScreepsM UpgraderError Unit
spawn name spawner creeps = lmap UpgraderCommonErr <$> mkSpawn parts ((_ <= 5) <$> count) opts name spawner creeps

upgradeRoomController :: Int -> Spawn -> Controller -> Creep -> ScreepsM CommonError Unit
upgradeRoomController threshold spawner roomController = \creep ->
  if level roomController < maxLevel || ticksToDowngrade roomController < threshold
  then moveToThen upgradeController roomController creep
  else returnEnergyToBase spawner creep

range :: Int
range = 4

run :: Spawn -> Creep -> ScreepsM UpgraderError Unit
run spawner creep =
  mkRun UpgraderCommonErr
        role
        (\crp -> lmap UpgraderCommonErr <$>
                 gatherEnergyThen shouldGather
                                  (maybe (\_ -> throwM TargetDoesNotExistErr)
                                         (upgradeRoomController 1000 spawner)
                                         cont)
                                  crp)
        creep
  where
    cont = controller $ room creep
    shouldGather :: Creep -> Effect Boolean
    shouldGather crp = pure $ not (isNear crp cont range) && freeCapacity crp > 0 || amtCarrying crp resource_energy == 0
