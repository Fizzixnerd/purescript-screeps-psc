module Screeps.Role.Harvester where

import Prelude

import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Screeps (Creep, Spawn, part_carry, part_move, part_work)
import Screeps.Creep (freeCapacity)
import Screeps.Monad (ScreepsM)
import Screeps.Role.Common (CommonError, Role(..), gatherEnergyThen, mkCount, mkRun, mkSpawn, returnEnergyToBase)
import Screeps.Spawn (SpawnOptions, spawnOpts)
import Screeps.Types (BodyPartType)
import Effect (Effect)

data HarvesterError =
  HarvesterCommonErr CommonError
derive instance genericHarvesterError :: Generic HarvesterError _
instance showHarvesterError :: Show HarvesterError where show = genericShow

type HarvesterMemory e =
  { role :: Role | e }

parts :: Array BodyPartType
parts = [part_carry, part_move, part_work]

role :: Role
role = Harvester

memory :: HarvesterMemory ()
memory = { role: role }

opts :: SpawnOptions (HarvesterMemory ())
opts = spawnOpts { memory = Just memory }

count :: Effect Int
count = mkCount role

spawn :: String -> Spawn -> Array Creep -> ScreepsM HarvesterError Unit
spawn name spawner creeps = lmap HarvesterCommonErr <$> mkSpawn parts ((_ <= 10) <$> count) opts name spawner creeps

run :: Spawn -> Creep -> ScreepsM HarvesterError Unit
run spawner = mkRun HarvesterCommonErr role (\crp -> lmap HarvesterCommonErr <$> (gatherEnergyThen (\c -> pure $ freeCapacity c > 0) (returnEnergyToBase spawner) crp))
