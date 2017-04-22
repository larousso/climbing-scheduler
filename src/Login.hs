{-# LANGUAGE OverloadedStrings #-}

module Login where

import Db
import Users

import Data.UUID
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Aeson
import Data.UUID.Aeson
import Control.Applicative

import Database.PostgreSQL.Simple
import Data.Pool(Pool)

data LoginForm = LoginForm { login:: Text, password:: Text } deriving (Show)

instance FromJSON LoginForm where
     parseJSON (Object v) = LoginForm <$>
                            v .:  "login"    <*>
                            v .:  "password"

instance ToJSON LoginForm where
     toJSON (LoginForm login password) =
         object ["login" .= login,
                 "password" .= password]


data Session = UserSession {userLogin:: Text} | Unauthorized deriving (Show)

doLogin :: Pool Connection -> LoginForm -> IO Session
doLogin pool (LoginForm l p) = do
  mayBeUser <- findUserByLogin pool l
  return $ toSession p mayBeUser
  where toSession pass (Just (User _ l p)) = if pass == p then UserSession{userLogin=l} else Unauthorized
        toSession pass Nothing = Unauthorized
