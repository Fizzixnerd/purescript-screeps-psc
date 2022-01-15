module Screeps.Role.Common where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (find)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Screeps (BodyPartType, Creep, ReturnCode, RoomObject, Spawn, TargetPosition(..), err_busy, err_not_enough_energy, err_not_in_range, find_sources, ok, resource_energy)
import Screeps.Creep (amtCarrying, freeCapacity, getMemory, harvestSource, moveTo, name, transferAmtToStructure, transferToStructure)
import Screeps.Monad (ScreepsM, ScreepsM')
import Screeps.Room (find')
import Screeps.RoomObject (room)
import Screeps.Spawn (energy, energyCapacity, spawnCreep', SpawnOptions)

throwM :: forall m err a. Applicative m => err -> ScreepsM' m err a
throwM = pure <<< Left

okM :: forall m err a. Applicative m => a -> ScreepsM' m err a
okM = pure <<< Right

checkOkM :: forall m. Applicative m => ReturnCode -> ScreepsM' m CommonError Unit
checkOkM rc | rc == ok = okM unit
            | otherwise = throwM (OtherErr rc)

data CommonError =
  TargetDoesNotExistErr
  | CreepWithNameExistsErr String
  | NotEnoughEnergyToSpawnCreepErr
  | SpawnBusyErr
  | SpawnEnergyFullErr
  | OtherErr ReturnCode
  | ShouldNotSpawnErr String
  | CreepRoleErr String
derive instance genericCommonError :: Generic CommonError _
instance showCommonError :: Show CommonError where show = genericShow

mkSpawn :: forall a. EncodeJson a => Array BodyPartType -> Effect Boolean -> SpawnOptions a -> String -> Spawn -> Array Creep -> ScreepsM CommonError Unit
mkSpawn parts condition opts = \desiredName spawner creeps -> do
  shouldSpawn <- condition
  if shouldSpawn then do
    let creep = find (\c -> name c == desiredName) creeps
    case creep of
      Just _ -> throwM $ CreepWithNameExistsErr desiredName
      Nothing -> do
        returnCode <- spawnCreep' opts parts (Just desiredName) spawner
        case returnCode of
          rc | rc == err_not_enough_energy -> throwM NotEnoughEnergyToSpawnCreepErr
             | rc == err_busy -> throwM SpawnBusyErr
             | otherwise -> checkOkM rc
    else throwM $ ShouldNotSpawnErr desiredName

mkRun :: forall err a. (DecodeJson a) => (CommonError -> err) -> (a -> String) -> String -> (Creep -> ScreepsM err Unit) -> Creep -> ScreepsM err Unit
mkRun fromCommonErr getRole role runner = \creep -> do
  isOk <- hasRole creep getRole role
  case isOk of
    Left _ -> okM unit
    Right false -> throwM $ fromCommonErr $ CreepRoleErr role
    Right true -> runner creep

hasRole :: forall a. (DecodeJson a) => Creep -> (a -> String) -> String -> ScreepsM JsonDecodeError Boolean
hasRole creep getRole roleName = do
  role <- getMemory creep
  case role of
    Left jsonErr -> throwM jsonErr
    Right mem -> okM (getRole mem == roleName)

gatherEnergyThen :: forall err. (CommonError -> err) -> (Creep -> ScreepsM err Boolean) -> (Creep -> ScreepsM err Unit) -> (Creep -> ScreepsM err Unit)
gatherEnergyThen fromCommonError shouldGather actionAfterGather = \creep -> do
  creepShouldGather <- shouldGather creep
  case creepShouldGather of
    Right true -> do
      let sources = find' (room creep) find_sources (const true)
      lmap fromCommonError <$> moveToFirstThen harvestSource creep sources
    Right false -> actionAfterGather creep
    Left _ -> actionAfterGather creep

moveToFirstThen :: forall a. (Creep -> RoomObject a -> Effect ReturnCode) -> Creep -> Array (RoomObject a) -> ScreepsM CommonError Unit
moveToFirstThen actionAfterMove = \creep targets -> do
  let first = head targets
  case first of
    Nothing -> throwM TargetDoesNotExistErr
    Just target -> do
      returnCode <- actionAfterMove creep target
      case returnCode of
        rc | rc == err_not_in_range -> moveTo creep (TargetObj target) >>= checkOkM
           | otherwise -> checkOkM rc

returnEnergyToBase :: forall err. (CommonError -> err) -> Spawn -> Creep -> ScreepsM err Unit
returnEnergyToBase fromCommonError spawner creep =
  if energy spawner < energyCapacity spawner
  then do
    let amt = amtCarrying creep resource_energy
    lmap fromCommonError <$> moveToFirstThen (\c s -> transferAmtToStructure c s resource_energy amt) creep [spawner] 
  else throwM $ fromCommonError SpawnEnergyFullErr

maxLevel :: Int
maxLevel = 8
