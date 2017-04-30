{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Slots where

import Db

import Data.Maybe
import Data.UUID
import Data.Time.Clock
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Data.UUID.Aeson
import Control.Applicative

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Pool(Pool)
import qualified Data.Text as T
import qualified Text.Read as R
import GHC.Generics

data Slot = Slot {
  id:: UUID,
  userId:: UUID,
  acceptMultipe:: Bool,
  start:: UTCTime,
  end:: UTCTime
}

instance FromJSON Slot where
     parseJSON = withObject "Slot" $ \v -> Slot <$>
                            v .:? "id" .!= Data.UUID.nil <*> -- the field "id" is optional
                            v .:? "userId" .!= Data.UUID.nil <*> -- the field "id" is optional
                            v .:? "acceptMultipe" .!= False <*>
                            v .:  "start"    <*>
                            v .:  "end"

instance ToJSON Slot where
     toJSON Slot{..} =
         object ["id" .= id,
                 "userId" .= userId,
                 "acceptMultipe" .= acceptMultipe,
                 "start" .= start,
                 "end" .= end]

newSlot:: UUID -> Bool -> UTCTime -> UTCTime -> Slot
newSlot = Slot Data.UUID.nil

slotScripts:: [Query]
slotScripts = [
  " CREATE TABLE IF NOT EXISTS \"slot\" ( \
    \  id uuid primary key DEFAULT uuid_generate_v4(), \
    \  user_id uuid references \"user\"(id), \
    \  accept_multiple bool not null, \
    \  slot_start timestamptz not null, \
    \  slot_end timestamptz not null, \
    \  CONSTRAINT valid_times CHECK (slot_end > slot_start), \
    \  UNIQUE (user_id, accept_multiple, slot_start, slot_end) \
    \); "
  ]

rowToSlot:: (UUID, UUID, Bool, UTCTime, UTCTime) -> Maybe Slot
rowToSlot (id, userid, acceptMultiple, start, end) = Just (Slot id userid acceptMultiple start end)

findSlot:: Pool Connection -> Slot -> IO (Maybe Slot)
findSlot pool (Slot _ userId acceptMultiple start end) =  do
        res <- fetch pool (userId, acceptMultiple, start, end) "SELECT * FROM \"slot\" WHERE user_id = ? AND accept_multiple = ? AND slot_start = ? AND slot_end = ?" :: IO [(UUID, UUID, Bool, UTCTime, UTCTime)]
        return $ slot res
        where slot [a] = rowToSlot a
              slot [] = Nothing

findSlotsByUserLogin:: Pool Connection -> Text -> IO [Slot]
findSlotsByUserLogin pool login =  do
        res <- fetch pool (Only login) "SELECT s.* FROM \"slot\" s JOIN \"user\" u ON u.id = s.user_id WHERE u.login = ? " :: IO [(UUID, UUID, Bool, UTCTime, UTCTime)]
        return $ mapMaybe rowToSlot res

createSlot:: Pool Connection -> Slot -> IO (Either Text Slot)
createSlot pool slot = do
      mayBeSlot <- findSlot pool slot
      case mayBeSlot of
        Nothing -> create pool slot
        Just s -> return $ Left "Slot already exists"

updateSlot :: Pool Connection -> UUID -> Slot -> IO (Either Text Slot)
updateSlot pool id (Slot _ userId acceptMultiple start end) = do
    uuid <- fetch pool (acceptMultiple, start, end, id) "UPDATE \"slot\" SET accept_multiple= ? AND slot_start=?, slot_end=? where id = ? RETURNING id " :: IO [Only UUID]
    return $ nslot uuid
    where nslot [Only id] = Right $ Slot id userId acceptMultiple start end
          nslot _ = Left "Error"

deleteSlot :: Pool Connection -> UUID -> IO ()
deleteSlot pool id = do
  _ <- execSql pool (Only id) "DELETE FROM \"slot\" where id = ? "
  return ()

create :: Pool Connection -> Slot -> IO (Either Text Slot)
create pool (Slot _ userId acceptMultiple start end) = do
  uuid <- fetch pool (userId, acceptMultiple, start, end) "INSERT INTO \"slot\" (user_id, accept_multiple, slot_start, slot_end) VALUES (?,?,?,?) RETURNING id " :: IO [Only UUID]
  return $ nslot uuid
  where nslot [Only id] = Right $ Slot id userId acceptMultiple start end
        nslot _ = Left "Error"

searchSimilarSlot:: Pool Connection -> UUID -> IO [Slot]
searchSimilarSlot pool userId = do
  slots <- fetch pool (Only userId) query :: IO [(UUID, UUID, Bool, UTCTime, UTCTime)]
  return $ mapMaybe rowToSlot slots
  where query = " \
  \ SELECT ss.* \
  \ FROM  \
  \   \"slot\" s,  \
  \   \"slot\" ss \
  \ WHERE \
  \  s.id = ? AND  \
  \  ss.id != s.id AND  \
  \  s.user_id != ss.user_id AND  \
  \  s.accept_multiple = ss.accept_multiple AND  \
  \  GREATEST(  extract(epoch from s.slot_start), extract(epoch from ss.slot_start)) < LEAST(extract(epoch from s.slot_end), extract(epoch from ss.slot_end)) \
  \ ORDER BY (LEAST(extract(epoch from s.slot_end), extract(epoch from ss.slot_end)) - GREATEST(extract(epoch from s.slot_start), extract(epoch from ss.slot_start))) DESC\
  \";
