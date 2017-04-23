{-# LANGUAGE OverloadedStrings #-}

module Session where

import Db
import Users

import Data.UUID
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import Data.HMAC (hmac_sha1)
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

newtype UserSession  = UserSession {userLogin:: Text}

instance FromJSON UserSession where
     parseJSON (Object v) = UserSession <$>
                            v .:  "userLogin"

instance ToJSON UserSession where
     toJSON (UserSession userLogin) =
         object ["userLogin" .= userLogin]


sessionCookie:: String -> UserSession -> BS.ByteString
sessionCookie secret us =
  let strSession = lazyToStrict $ encode us
      sep = lazyToStrict $ encodeUtf8 "-"
  in base64 $ mconcat [strSession, sep, sign (BSC8.pack secret) strSession]
  where lazyToStrict = BS.concat . BSL.toChunks

sign:: BS.ByteString -> BS.ByteString -> BS.ByteString
sign secret text = BS.pack $ hmac_sha1 (BS.unpack secret) (BS.unpack text)

base64:: BS.ByteString -> BS.ByteString
base64 = B64.encode

doLogin :: Pool Connection -> LoginForm -> IO (Maybe UserSession)
doLogin pool (LoginForm l p) = do
  mayBeUser <- findUserByLogin pool l
  return $ toSession p mayBeUser
  where toSession pass (Just (User _ l p)) = if pass == p then Just UserSession{userLogin=l} else Nothing
        toSession pass Nothing = Nothing
