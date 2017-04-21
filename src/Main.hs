{-# LANGUAGE OverloadedStrings #-}

module Main where

import Db
import Users

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404, ok200, badRequest400)
import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import Data.Word
import Data.Aeson
import Database.PostgreSQL.Simple


makeDbConfig :: C.Config -> IO (Maybe Db.DbConfig)
makeDbConfig conf = do
  name <- C.lookup conf "database.name" :: IO (Maybe String)
  user <- C.lookup conf "database.user" :: IO (Maybe String)
  password <- C.lookup conf "database.password" :: IO (Maybe String)
  host <- C.lookup conf "database.host" :: IO (Maybe String)
  port <- C.lookup conf "database.port" :: IO (Maybe Word16)
  return $ DbConfig
                    <$> host
                    <*> port
                    <*> name
                    <*> user
                    <*> password

test:: Middleware
test m = m

auth :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
auth app req respond = app req respond

main:: IO ()
main = do
  loadedConf <- C.load [C.Required "src/application.conf"]
  dbConf <- makeDbConfig loadedConf
  case dbConf of
    Nothing -> putStrLn "No database configuration found, terminating..."
    Just conf -> do
        pool <- createPool (newConn conf) close 1 40 10
        scotty 3000 $ do
          middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
          middleware $ auth

          get "/users" $ do
              users <- liftIO $ findAllUsers pool
              status ok200
              Web.Scotty.json (users :: [User])
          post "/users" $ do
              u <- jsonData
              successOrFailure <- liftIO $ createUser pool u
              case successOrFailure of
                Left err -> do
                    status badRequest400
                    Web.Scotty.text err
                Right u -> do
                    status created201
                    Web.Scotty.json (u :: User)
          get "/users/:login" $ do
              login <- param "login"
              mayBeUser <- liftIO $ findUserByLogin pool login
              case mayBeUser of
                Nothing -> 
                  status notFound404
                Just user -> do
                  status ok200
                  Web.Scotty.json (user :: User)
