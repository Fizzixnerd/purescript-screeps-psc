module Screeps.Role.Common where

import Prelude

import Data.Argonaut.Core (fromString, toString)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array (foldM)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (find)
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (Error)
import Foreign.Object (values)
import Screeps (BodyPartType, Creep, ReturnCode, RoomObject, Spawn, TargetPosition(..), ConstructionSite, err_busy, err_not_enough_energy, err_not_in_range, find_sources, ok, resource_energy)
import Screeps.Creep (amtCarrying, build, getMemoryBy, harvestSource, moveTo, name, transferAmtToStructure)
import Screeps.Game (creeps, getGameGlobal)
import Screeps.Monad (ScreepsM, ScreepsM')
import Screeps.Room (find')
import Screeps.RoomObject (pos, room)
import Screeps.RoomPosition (closestPathOpts, findClosestByPath, inRangeTo)
import Screeps.Spawn (energy, energyCapacity, spawnCreep', SpawnOptions)
import Screeps.Types (FindContext(..))

throwM :: forall m err a. Applicative m => err -> ScreepsM' m err a
throwM = pure <<< Left

okM :: forall m err a. Applicative m => a -> ScreepsM' m err a
okM = pure <<< Right

checkOkM :: forall m. Applicative m => ReturnCode -> ScreepsM' m CommonError Unit
checkOkM rc | rc == ok = okM unit
            | otherwise = throwM (OtherErr rc)

data Role =
  Harvester
  | Upgrader
  | Builder
derive instance genericRole :: Generic Role _
instance eqRole :: Eq Role where eq = genericEq
instance showRole :: Show Role where show = genericShow

instance roleEncodeJson :: EncodeJson Role where
  encodeJson Harvester = fromString "\"harvester\""
  encodeJson Upgrader = fromString "\"upgrader\""
  encodeJson Builder = fromString "\"builder\""

instance roleDecodeJson :: DecodeJson Role where
  decodeJson r = maybe (Left (TypeMismatch "Role")) pure $
                 (case _ of
                     "harvester" -> pure Harvester
                     "upgrader" -> pure Upgrader
                     "builder" -> pure Builder
                     _ -> Nothing)
                 =<< toString r

data CommonError =
  TargetDoesNotExistErr
  | CreepWithNameExistsErr String
  | NotEnoughEnergyToSpawnCreepErr
  | SpawnBusyErr
  | SpawnEnergyFullErr
  | ShouldNotSpawnErr String
  | CreepRoleErr Role
  | JsonError JsonDecodeError
  | PathFindingError Error
  | OtherErr ReturnCode
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

mkRun :: forall err. (CommonError -> err) -> Role -> (Creep -> ScreepsM err Unit) -> Creep -> ScreepsM err Unit
mkRun fromCommonErr role runner = \creep -> do
  creepHasAppropriateRole <- creep `hasRole` role
  case creepHasAppropriateRole of
    Left err -> throwM $ fromCommonErr $ JsonError err
    Right false -> throwM $ fromCommonErr $ CreepRoleErr role
    Right true -> runner creep

hasRole :: Creep -> Role -> ScreepsM JsonDecodeError Boolean
hasRole creep roleName = do
  (role :: Either _ Role) <- getMemoryBy "role" creep
  case role of
    Left jsonErr -> throwM jsonErr
    Right r -> okM (r == roleName)

mkCount :: Role -> Effect Int
mkCount role = do
  creeps <- values <<< creeps <$> getGameGlobal
  foldM (\acc c -> do
            isRoleE <- c `hasRole` role
            case isRoleE of
              Left _ -> pure acc
              Right false -> pure acc
              Right true -> pure (acc + 1))
        0
        creeps

gatherEnergyThen :: (Creep -> Effect Boolean) -> (Creep -> ScreepsM CommonError Unit) -> Creep -> ScreepsM CommonError Unit
gatherEnergyThen shouldGather actionAfterGather = \creep -> do
  creepShouldGather <- shouldGather creep
  if creepShouldGather
    then
      let closestSrc = findClosestByPath (pos creep) (OfType find_sources) in
      case closestSrc of
        Left err -> throwM $ PathFindingError err
        Right Nothing -> throwM TargetDoesNotExistErr
        Right (Just src) -> moveToThen harvestSource src creep
    else actionAfterGather creep

moveToThen :: forall a. (Creep -> RoomObject a -> Effect ReturnCode) -> RoomObject a -> Creep -> ScreepsM CommonError Unit
moveToThen actionAfterMove = \target creep -> do
  returnCode <- actionAfterMove creep target
  case returnCode of
    _ | returnCode == err_not_in_range -> moveTo creep (TargetObj target) >>= checkOkM
      | otherwise -> checkOkM returnCode

moveToBestThen :: forall a. (Creep -> RoomObject a -> Effect Int) -> (Creep -> RoomObject a -> Effect ReturnCode) -> Creep -> Array (RoomObject a) -> ScreepsM CommonError Unit
moveToBestThen judgeGoodness actionAfterMove = \creep targets -> do
  best <- foldM (\acc x -> do
                    goodness <- judgeGoodness creep x
                    case acc of
                      Just (Tuple goodnessAcc _) -> if goodness > goodnessAcc then pure (Just (Tuple goodness x)) else pure acc
                      Nothing -> pure (Just (Tuple goodness x)))
                Nothing
                targets
  case best of
    Nothing -> throwM TargetDoesNotExistErr
    Just (Tuple _ target) -> moveToThen actionAfterMove target creep

moveToFirstThen :: forall a. (Creep -> RoomObject a -> Effect ReturnCode) -> Creep -> Array (RoomObject a) -> ScreepsM CommonError Unit
moveToFirstThen = moveToBestThen (\_ _ -> pure 0)

returnEnergyToBase :: Spawn -> Creep -> ScreepsM CommonError Unit
returnEnergyToBase spawner creep =
  if energy spawner < energyCapacity spawner
  then do
    let amt = amtCarrying creep resource_energy
    moveToThen (\c s -> transferAmtToStructure c s resource_energy amt) spawner creep
  else throwM SpawnEnergyFullErr

buildConstructionSite :: ConstructionSite -> Creep -> ScreepsM CommonError Unit
buildConstructionSite = moveToThen build

maxLevel :: Int
maxLevel = 8

isNear :: forall a b. RoomObject a -> Maybe (RoomObject b) -> Int -> Boolean
isNear x my range = maybe false (\y -> inRangeTo (pos x) (TargetObj y) range) my
