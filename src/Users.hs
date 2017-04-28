{-# LANGUAGE OverloadedStrings #-}

module Users where

import Db

import Data.UUID
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Data.UUID.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple

import Data.Pool(Pool)
import GHC.Int

data User = User {
      id:: UUID,
      login:: Text,
      password:: Text -- id title bodyText
} deriving (Show)

instance FromJSON User where
     parseJSON (Object v) = User <$>
                            v .:? "id" .!= Data.UUID.nil <*> -- the field "id" is optional
                            v .:  "login"    <*>
                            v .:  "password"

instance ToJSON User where
     toJSON (User id surname password) =
         object ["id" .= id,
                 "login" .= surname]

userScripts:: [Query]
userScripts = [
  "CREATE TABLE IF NOT EXISTS \"user\" ( \
  \  id uuid primary key DEFAULT uuid_generate_v4(), \
  \  login text not null unique, \
  \  name text, \
  \  surname text, \
  \  level text, \
  \  password text not null, \
  \  timestamp timestamp DEFAULT current_timestamp ); ",
  "CREATE UNIQUE INDEX IF NOT EXISTS \"user_login\" ON \"user\"(login) ;"
  ]

createTableUser:: Pool Connection -> IO()
createTableUser pool = do
  _ <- execSqlSimple pool script
  _ <- execSqlSimple pool createIndex
  return ()
  where
    createIndex = "CREATE UNIQUE INDEX IF NOT EXISTS \"user_login\" ON \"user\"(login) ;"
    script = "CREATE TABLE IF NOT EXISTS \"user\" ( \
    \  id uuid primary key DEFAULT uuid_generate_v4(), \
    \  login text not null unique, \
    \  name text, \
    \  surname text, \
    \  level text, \
    \  password text not null, \
    \  timestamp timestamp DEFAULT current_timestamp \
    \);"

rowToUser:: (UUID, Text, Text) -> Maybe User
rowToUser (id, login, password) = Just (User id login password)

findUserByLogin :: Pool Connection -> Text -> IO (Maybe User)
findUserByLogin pool login = do
        res <- fetch pool (Only login) "SELECT id, login, password FROM \"user\" WHERE login = ?" :: IO [(UUID, Text, Text)]
        return $ user res
        where user [a] = rowToUser a
              user _ = Nothing

findUserById :: Pool Connection -> UUID -> IO (Maybe User)
findUserById pool id = do
        res <- fetch pool (Only id) "SELECT id, login, password FROM \"user\" WHERE id = ?" :: IO [(UUID, Text, Text)]
        return $ user res
        where user [a] = rowToUser a
              user _ = Nothing

findAllUsers :: Pool Connection -> IO [User]
findAllUsers pool = do
        res <- fetchSimple pool "SELECT id, login, password FROM \"user\" ORDER BY id DESC" :: IO [(UUID, Text, Text)]
        return $ Prelude.map (\(id, login, password) -> User id login password) res

createUser :: Pool Connection -> User -> IO (Either Text User)
createUser pool (User _ login password) = do
      mayBeUser <- findUserByLogin pool login
      case mayBeUser of
        Nothing -> create pool (User Data.UUID.nil login password)
        Just u -> return $ Left "User already exists"

createOrUpdateUser :: Pool Connection -> Text -> User -> IO (Either Text User)
createOrUpdateUser pool userLogin (User _ login password) = do
      mayBeUser <- findUserByLogin pool userLogin
      case mayBeUser of
        Nothing -> create pool (User Data.UUID.nil userLogin password)
        Just u -> update pool (User Data.UUID.nil userLogin password)

create :: Pool Connection -> User -> IO (Either Text User)
create pool (User _ login password) = do
    uuid <- fetch pool [login, password] "INSERT INTO \"user\" (login, password) VALUES (?,?) RETURNING id " :: IO [Only UUID]
    return $ user uuid
    where user [Only id] = Right $ User id login password
          user _ = Left "Error"

update :: Pool Connection -> User -> IO (Either Text User)
update pool (User _ login password) = do
    uuid <- fetch pool [login, password, login] "UPDATE \"user\" SET login=?, password=? where login = ? RETURNING id " :: IO [Only UUID]
    return $ user uuid
    where user [Only id] = Right $ User id login password
          user _ = Left "Error"

deleteUser :: Pool Connection -> Text -> IO ()
deleteUser pool login = do
    _ <- execSql pool (Only login) "DELETE FROM \"user\" where login = ? "
    return ()
