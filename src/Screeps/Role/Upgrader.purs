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
import Screeps.Role.Common (CommonError, gatherEnergyThen, maxLevel, mkRun, mkSpawn, moveToFirstThen, okM, returnEnergyToBase, throwM, Role(..))
import Screeps.Room (controller)
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (inRangeTo, isNearTo)
import Screeps.Spawn (SpawnOptions, spawnOpts)
import Screeps.Types (BodyPartType, TargetPosition(..))

data UpgraderError =
  NotEnoughEnergyToSpawnCreepErr
  | CreepWithNameExistsErr
  | SpawnBusyErr
  | SpawnEnergyFullErr
  | UpgraderCommonErr CommonError
  | NoControllerErr

derive instance genericUpgraderError :: Generic UpgraderError _
instance showUpgraderError :: Show UpgraderError where show = genericShow

type UpgraderMemory e =
  { role :: Role | e }

parts :: Array BodyPartType
parts = [part_carry, part_move, part_work]

role :: Role
role = Upgrader

memory :: UpgraderMemory ()
memory = { role: role }

opts :: SpawnOptions (UpgraderMemory ())
opts = spawnOpts { memory = Just memory }

spawn :: String -> Spawn -> Array Creep -> ScreepsM UpgraderError Unit
spawn name spawner creeps = lmap UpgraderCommonErr <$> mkSpawn parts (pure true) opts name spawner creeps

upgradeRoomController :: Int -> Spawn -> Controller -> Creep -> ScreepsM UpgraderError Unit
upgradeRoomController threshold spawner roomController = \creep ->
  if level roomController < maxLevel || ticksToDowngrade roomController < threshold
  then lmap UpgraderCommonErr <$> moveToFirstThen upgradeController creep [roomController]
  else returnEnergyToBase UpgraderCommonErr spawner creep

range :: Int
range = 4

run :: Spawn -> Creep -> ScreepsM UpgraderError Unit
run spawner creep =
  mkRun UpgraderCommonErr
        role
        (gatherEnergyThen UpgraderCommonErr
                          shouldGather
                          (maybe (\_ -> throwM NoControllerErr)
                                 (upgradeRoomController 1000 spawner)
                                 cont))
        creep
  where
    cont = controller $ room creep
    shouldGather :: forall err. Creep -> ScreepsM err Boolean
    shouldGather cr = okM $ not isNearController && hasCapacity cr || amtCarrying cr resource_energy == 0
      where
        isNearController = maybe false (\co -> inRangeTo (pos cr) (TargetObj co) range) cont
        hasCapacity cre = maybe false (const $ freeCapacity cre > 0) cont
