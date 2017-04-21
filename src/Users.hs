{-# LANGUAGE OverloadedStrings #-}

module Users where

import Db
import Data.UUID
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Data.UUID.Aeson
import Control.Applicative

import Web.Scotty.Internal.Types (ActionT)
import GHC.Generics (Generic)
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple
import Data.Pool(Pool, createPool, withResource)
import Data.Time (LocalTime)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import GHC.Int

data User = User UUID Text Text -- id title bodyText
     deriving (Show)

instance FromJSON User where
     parseJSON (Object v) = User <$>
                            v .:? "id" .!= Data.UUID.nil <*> -- the field "id" is optional
                            v .:  "login"    <*>
                            v .:  "password"

instance ToJSON User where
     toJSON (User id surname password) =
         object ["id" .= id,
                 "login" .= surname]

findUserByLogin :: Pool Connection -> Text -> IO (Maybe User)
findUserByLogin pool login = do
        res <- fetch pool (Only login) "SELECT id, login, password FROM \"user\" WHERE login = ?" :: IO [(UUID, TL.Text, TL.Text)]
        return $ user res
        where user [(id, login, password)] = Just (User id login password)
              user _ = Nothing

findAllUsers :: Pool Connection -> IO [User]
findAllUsers pool = do
        res <- fetchSimple pool "SELECT id, login, password FROM \"user\" ORDER BY id DESC" :: IO [(UUID, TL.Text, TL.Text)]
        return $ Prelude.map (\(id, login, password) -> User id login password) res

createUser :: Pool Connection -> User -> IO (Either Text User)
createUser pool (User _ login password) = do
      mayBeUser <- findUserByLogin pool login
      case mayBeUser of
        Nothing -> create pool (User Data.UUID.nil login password)
        Just u -> return $ Left "User already exists"

create :: Pool Connection -> User -> IO (Either Text User)
create pool (User _ login password) = do
    uuid <- fetch pool [login, password] "INSERT INTO \"user\" (login, password) VALUES (?,?) RETURNING id " :: IO [Only UUID]
    return $ user uuid
    where user [Only id] = Right $ User id login password
          user _ = Left "Error"
