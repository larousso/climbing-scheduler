{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Slots where

import Db

import Data.Maybe
import Data.UUID
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Data.UUID.Aeson
import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Pool(Pool)
import qualified Data.Text as T
import qualified Text.Read as R
import GHC.Generics

data Day = Monday | Thuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read)

dayFromText:: T.Text -> Day
dayFromText t = case t of
  "Monday" -> Monday
  "Thuesday" -> Thuesday
  "Wednesday" -> Wednesday
  "Thursday" -> Thursday
  "Friday" -> Friday
  "Saturday" -> Saturday
  "Sunday" -> Sunday

instance FromField Day where
  fromField field mdata = do
    x <- fromField field mdata
    return $ dayFromText $ toStrict x

instance ToField Day where
  toField d = toField $ show d

instance FromJSON Day where
     parseJSON = withText "Day" $ \t -> return $ dayFromText t

instance ToJSON Day where
     toJSON d = toJSON $ T.pack $ show d

data Minutes = Zero | Half deriving (Generic)

minutesToInt:: Minutes -> Int
minutesToInt Zero = 0
minutesToInt Half = 30

minutesFromInt:: Int -> Minutes
minutesFromInt 0 = Zero
minutesFromInt 30 = Half

instance FromField Minutes where
  fromField field mdata = do
    x <- fromField field mdata
    return $ minutesFromInt x

instance ToField Minutes where
  toField m = toField $ minutesToInt m

instance FromJSON Minutes where
  parseJSON = withScientific "Minutes" $ \t -> case t of
    0 -> return Zero
    30 -> return Half

instance ToJSON Minutes where
  toJSON Zero = toJSON (Number 0)
  toJSON Half = toJSON (Number 30)

type Hours = Int

data SlotTime = SlotTime {
  hour:: Hours,
  minutes:: Minutes,
  value:: !Int
} deriving (Generic)

newSlotTime:: Hours -> Minutes -> SlotTime
newSlotTime h m = SlotTime h m (calcValue h m)

instance FromJSON SlotTime where
     parseJSON = withObject "Slot" $ \v -> newSlotTime <$>
                            v .: "hour" <*>
                            v .: "minutes"

instance ToJSON SlotTime where
     toJSON SlotTime{..} =
         object ["hour" .= hour,
                 "minutes" .= minutes,
                 "value" .= value]


calcValue:: Hours -> Minutes -> Int
calcValue h Zero = h * 10
calcValue h Half = h * 10 + 5

data Slot = Slot {
  id:: UUID,
  userId:: UUID,
  acceptMultipe:: Bool,
  day:: Day,
  start:: SlotTime,
  end:: SlotTime
}

instance FromJSON Slot where
     parseJSON = withObject "Slot" $ \v -> Slot <$>
                            v .:? "id" .!= Data.UUID.nil <*> -- the field "id" is optional
                            v .:? "userId" .!= Data.UUID.nil <*> -- the field "id" is optional
                            v .:? "acceptMultipe" .!= False <*>
                            v .:  "day"    <*>
                            v .:  "start"    <*>
                            v .:  "end"

instance ToJSON Slot where
     toJSON Slot{..} =
         object ["id" .= id,
                 "userId" .= userId,
                 "day" .= day,
                 "acceptMultipe" .= acceptMultipe,
                 "start" .= start,
                 "end" .= end]

newSlot:: UUID -> Bool -> Day -> Hours -> Minutes -> Hours -> Minutes -> Slot
newSlot = slot Data.UUID.nil

slot:: UUID -> UUID -> Bool -> Day -> Hours -> Minutes -> Hours -> Minutes -> Slot
slot id userId acceptMultipe day startHours startMinutes endHours endMinutes =
  Slot id userId acceptMultipe day (SlotTime startHours startMinutes (calcValue startHours startMinutes)) (SlotTime endHours endMinutes (calcValue endHours endMinutes))

createTableSlot:: Pool Connection -> IO ()
createTableSlot pool = do
  _ <- execSqlSimple pool script
  return ()
  where script = " CREATE TABLE IF NOT EXISTS \"slot\" ( \
    \  id uuid primary key DEFAULT uuid_generate_v4(), \
    \  user_id uuid references \"user\"(id), \
    \  accept_multiple bool not null, \
    \  day text not null, \
    \  slot_start_hours int not null, \
    \  slot_start_minutes int not null, \
    \  slot_start_value int not null, \
    \  slot_end_hours int not null, \
    \  slot_end_minutes int not null, \
    \  slot_end_value int not null, \
    \  CONSTRAINT valid_times CHECK (slot_end_value > slot_start_value), \
    \  UNIQUE (user_id, day, slot_start_value, slot_end_value) \
    \); "


rowToSlot:: (UUID, UUID, Bool, Day, Int, Minutes, Int, Int, Minutes, Int) -> Maybe Slot
rowToSlot (id, userid, acceptMultiple, day, startHour, startMinutes, startValue, endHour, endMinutes, endValue) = Just (Slot id userid acceptMultiple day (SlotTime  startHour startMinutes startValue) (SlotTime  endHour endMinutes endValue))

findSlot:: Pool Connection -> Slot -> IO (Maybe Slot)
findSlot pool (Slot _ userId acceptMultiple day (SlotTime _ _ slotStart) (SlotTime _ _ slotEnd)) =  do
        res <- fetch pool (userId, acceptMultiple, day, slotStart, slotEnd) "SELECT * FROM \"slot\" WHERE user_id = ? AND accept_multiple = ? AND day = ? AND slot_start_value = ? AND slot_end_value = ?" :: IO [(UUID, UUID, Bool, Day, Int, Minutes, Int, Int, Minutes, Int)]
        return $ slot res
        where slot [a] = rowToSlot a
              slot [] = Nothing

findSlotsByUserLogin:: Pool Connection -> Text -> IO [Slot]
findSlotsByUserLogin pool login =  do
        res <- fetch pool (Only login) "SELECT s.* FROM \"slot\" s JOIN \"user\" u ON u.id = s.user_id WHERE u.login = ? " :: IO [(UUID, UUID, Bool, Day, Int, Minutes, Int, Int, Minutes, Int)]
        return $ mapMaybe rowToSlot res

createSlot:: Pool Connection -> Slot -> IO (Either Text Slot)
createSlot pool slot = do
      mayBeSlot <- findSlot pool slot
      case mayBeSlot of
        Nothing -> create pool slot
        Just s -> return $ Left "Slot already exists"

updateSlot :: Pool Connection -> UUID -> Slot -> IO (Either Text Slot)
updateSlot pool id (Slot _ userId acceptMultiple day (SlotTime startHour startMinutes _) (SlotTime endHour endMinutes _)) = do
    uuid <- fetch pool (acceptMultiple, day, startHour, startMinutes, calcValue startHour startMinutes, endHour, endMinutes, calcValue endHour endMinutes, id) "UPDATE \"slot\" SET accept_multiple= ? AND day=?, slot_start_hours=?, slot_start_minutes=?, slot_start_value=?, slot_end_hours=?, slot_end_minutes=?, slot_end_value=? where id = ? RETURNING id " :: IO [Only UUID]
    return $ nslot uuid
    where nslot [Only id] = Right $ slot id userId acceptMultiple day startHour startMinutes endHour endMinutes
          nslot _ = Left "Error"

deleteSlot :: Pool Connection -> UUID -> IO ()
deleteSlot pool id = do
  _ <- execSql pool (Only id) "DELETE FROM \"slot\" where id = ? "
  return ()

create :: Pool Connection -> Slot -> IO (Either Text Slot)
create pool (Slot _ userId acceptMultiple day (SlotTime startHour startMinutes _) (SlotTime endHour endMinutes _)) = do
  uuid <- fetch pool (userId, acceptMultiple, day, startHour, startMinutes, calcValue startHour startMinutes, endHour, endMinutes, calcValue endHour endMinutes) "INSERT INTO \"slot\" (user_id, accept_multiple, day, slot_start_hours, slot_start_minutes, slot_start_value, slot_end_hours, slot_end_minutes, slot_end_value) VALUES (?,?,?,?,?,?,?,?,?) RETURNING id " :: IO [Only UUID]
  return $ nslot uuid
  where nslot [Only id] = Right $ slot id userId acceptMultiple day startHour startMinutes endHour endMinutes
        nslot _ = Left "Error"

searchSimilarSlot:: Pool Connection -> UUID -> IO [Slot]
searchSimilarSlot pool userId = do
  slots <- fetch pool (Only userId) query :: IO [(UUID, UUID, Bool, Day, Int, Minutes, Int, Int, Minutes, Int)]
  return $ mapMaybe rowToSlot slots
  where query = " \
  \ SELECT ss.* \
  \ FROM  \
  \   \"slot\" s,  \
  \   \"slot\" ss \
  \ WHERE \
  \  s.id != ? AND  \
  \  s.user_id != ss.user_id AND  \
  \  s.accept_multiple = ss.accept_multiple AND  \
  \  s.day = ss.day AND  \
  \  s.slot_start_value >= ss.slot_start_value AND  \
  \  s.slot_end_value <= ss.slot_end_value \
  \";
