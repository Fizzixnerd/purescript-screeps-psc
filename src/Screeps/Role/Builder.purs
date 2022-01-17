module Screeps.Role.Builder where

import Prelude

import Data.Array as A
import Data.Array.Partial (head)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Screeps (find_construction_sites, part_carry, part_move, part_work)
import Screeps.Constants (resource_energy)
import Screeps.Creep (amtCarrying, freeCapacity)
import Screeps.Monad (ScreepsM)
import Screeps.Role.Common (CommonError, Role(..), buildConstructionSite, gatherEnergyThen, isNear, mkCount, mkRun, mkSpawn)
import Screeps.Room (find')
import Screeps.RoomObject (room)
import Screeps.Spawn (SpawnOptions, spawnOpts)
import Screeps.Types (BodyPartType, Creep, Spawn, ConstructionSite)

data BuilderError =
  BuilderCommonErr CommonError
derive instance genericBuilderError :: Generic BuilderError _
instance showBuilderError :: Show BuilderError where show = genericShow

type BuilderMemory e =
  { role :: Role | e }

role :: Role
role = Builder

parts :: Array BodyPartType
parts = [part_work, part_carry, part_move]

count :: Effect Int
count = mkCount role

memory :: BuilderMemory ()
memory = { role: role }

opts :: SpawnOptions (BuilderMemory ())
opts = spawnOpts { memory = Just memory }

spawn :: String -> Spawn -> Array Creep -> ScreepsM BuilderError Unit
spawn name spawner creeps = lmap BuilderCommonErr <$> mkSpawn parts ((_ <= 2) <$> count) opts name spawner creeps

range :: Int
range = 4

run :: Creep -> ScreepsM BuilderError Unit
run creep =
  mkRun BuilderCommonErr
        role
        (\crp -> lmap BuilderCommonErr <$>
                 let mSite = A.head $ thingsToBuild crp in
                 gatherEnergyThen (shouldGather mSite) (buildConstructionSite (unsafePartial head $ thingsToBuild crp)) crp)
        creep
  where
    thingsToBuild crp = find' (room crp) find_construction_sites (const true)
    shouldGather :: Maybe ConstructionSite -> Creep -> Effect Boolean
    shouldGather s crp = pure $ isJust s && (freeCapacity crp > 0 && not (isNear crp s range) || amtCarrying crp resource_energy == 0)
