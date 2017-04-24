{-# LANGUAGE OverloadedStrings #-}

module Session where

import Db
import Users
import Http

import Data.UUID

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC8
import Data.ByteString.Internal (unpackBytes)

import Data.HMAC (hmac_sha1)
import Data.Aeson
import Data.UUID.Aeson
import Control.Applicative
import Database.PostgreSQL.Simple
import Data.Pool(Pool)
import Web.Scotty
import Web.Cookie
import qualified Data.List as LST
import qualified Data.Text as T
import qualified Data.Text.Internal as IT
import GHC.Word (Word8)

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
  in B64.encode $ mconcat [strSession, sep, sign (BSC8.pack secret) strSession]
  where lazyToStrict = BS.concat . BSL.toChunks

sign:: BS.ByteString -> BS.ByteString -> BS.ByteString
sign secret text = BS.pack $ hmac_sha1 (BS.unpack secret) (BS.unpack text)

readSession:: String -> ActionM (Either String UserSession)
readSession secret = do
  cookies <- getCookies
  case cookies >>= findSessionCookie of
    Just (k, v) -> return $ readCookie secret v
    Nothing -> return $ Left "No session"
  where findSessionCookie = LST.find (\(k, v) -> k == "session")

readCookie:: String -> IT.Text -> Either String UserSession
readCookie secret t =
  b64DecodeLazy t >>= extractSessionAndHash >>= verifyCookie (BSC8.pack secret) >>= strToUserSession
  where
    b64DecodeLazy t = B64.decode $ BSC8.pack $ unpack $ fromStrict t
  --B64.decode .

extractSessionAndHash:: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
extractSessionAndHash cookie = case splitCookie cookie of
  [a, b] -> Right (a, b)
  _ -> Left "Error parsing cookie"
  where splitCookie = BS.split (BS.c2w '-')

verifyCookie:: BS.ByteString -> (BS.ByteString, BS.ByteString) -> Either String (BS.ByteString, BS.ByteString)
verifyCookie secret (strS, hash) =
  if hashCookie == hash
  then  Right (strS, hash)
  else Left "Invalid cookie"
  where hashCookie = sign secret strS

strToUserSession:: (BS.ByteString, BS.ByteString) -> Either String UserSession
strToUserSession (str, hash) =
  let mbSession = decode $ BSL.fromStrict str :: Maybe UserSession
  in case mbSession of
    Just us -> Right us
    Nothing -> Left "Error decoding session"

-- handleCookie c = Right $ BS.unpack $ T.splitOn "-" c


doLogin :: Pool Connection -> LoginForm -> IO (Maybe UserSession)
doLogin pool (LoginForm l p) = do
  mayBeUser <- findUserByLogin pool l
  return $ toSession p mayBeUser
  where toSession pass (Just (User _ l p)) = if pass == p then Just UserSession{userLogin=l} else Nothing
        toSession pass Nothing = Nothing
