module Screeps.Role.Harvester where

import Prelude

import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Screeps (Creep, Spawn, part_carry, part_move, part_work)
import Screeps.Creep (freeCapacity)
import Screeps.Monad (ScreepsM)
import Screeps.Role.Common (CommonError, gatherEnergyThen, mkRun, mkSpawn, okM, returnEnergyToBase)
import Screeps.Spawn (SpawnOptions, spawnOpts)
import Screeps.Types (BodyPartType)

data HarvesterError =
  HarvesterCommonErr CommonError
derive instance genericHarvesterError :: Generic HarvesterError _
instance showHarvesterError :: Show HarvesterError where show = genericShow

type HarvesterMemory =
  { role :: String }

getRole :: HarvesterMemory -> String
getRole m = m.role

parts :: Array BodyPartType
parts = [part_carry, part_move, part_work]

role :: String
role = "harvester"

memory :: HarvesterMemory
memory = { role: role }

opts :: SpawnOptions HarvesterMemory
opts = spawnOpts { memory = Just memory }

spawn :: String -> Spawn -> Array Creep -> ScreepsM HarvesterError Unit
spawn name spawner creeps = lmap HarvesterCommonErr <$> mkSpawn parts (pure true) opts name spawner creeps

run :: Spawn -> Creep -> ScreepsM HarvesterError Unit
run spawner = mkRun HarvesterCommonErr getRole role (gatherEnergyThen HarvesterCommonErr (\c -> okM $ freeCapacity c > 0) (returnEnergyToBase HarvesterCommonErr spawner))
