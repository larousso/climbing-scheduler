{-# LANGUAGE OverloadedStrings #-}

import Db
import Users
import Slots
import Session
import Http

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT)
import Network.Wai
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status (created201, internalServerError500, notFound404, ok200, badRequest400, noContent204, unauthorized401)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Pool(Pool, createPool, withResource)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.Word
import Data.Aeson
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)



data AppConfig = AppConfig {
  secret:: String,
  dbConfig:: Db.DbConfig,
  port:: Int
}

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

loadPort:: IO (Maybe Int)
loadPort = do
  port <- lookupEnv "PORT"
  let e = case port of
         Nothing -> Just 3000
         Just s -> Just (read s :: Int)
  return e


loadSecret::  C.Config -> IO (Maybe String)
loadSecret conf = C.lookup conf "secret" :: IO (Maybe String)

loadConfig :: C.Config -> IO (Maybe AppConfig)
loadConfig conf = do
  dbConfig <- makeDbConfig conf
  secret <- loadSecret conf
  port <- loadPort
  return $ AppConfig <$> secret <*> dbConfig <*> port

auth :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
auth app = app

jsonUnauthorized:: String -> ActionM ()
jsonUnauthorized err = do
  status unauthorized401
  Web.Scotty.json $ object ["error" .= err]

badRequest:: String -> ActionM ()
badRequest err = do
  status badRequest400
  Web.Scotty.json $ object ["error" .= err]


checkAuth :: String -> TL.Text -> (UserSession -> ActionM ()) -> ActionM ()
checkAuth secret login action = do
   s <- readSession secret
   case s of
     Left err -> jsonUnauthorized err
     Right (UserSession l UserRole) ->
        if login == l
        then action (UserSession l UserRole)
        else jsonUnauthorized "Not authorized"
     Right (UserSession l Admin) ->
        action (UserSession l UserRole)

initDb:: Pool Connection -> IO ()
initDb pool = do
  _ <- traverse (execSqlSimple pool) (
        ["CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";"] ++
        userScripts ++
        slotScripts
      )
  return ()

main:: IO ()
main = do
  loadedConf <- C.load [C.Required "conf/application.conf"]
  conf <- loadConfig loadedConf
  case conf of
    Nothing -> putStrLn "Error getting config, terminating ..."
    Just (AppConfig secret dbConf port) -> do
        pool <- createPool (newConn dbConf) close 1 40 10
        _ <- initDb pool
        scotty port $ do
          middleware $ staticPolicy (noDots >-> addBase "static") -- serve static files
          middleware auth

          get "/" $ do
            html $ mconcat [ " <!doctype> \
                              \ <html> \
                              \ <head> \
                              \ <title>purescript-webpack-example</title> \
                              \ <meta name=\"viewport\" content=\"width=device-width\">\
                              \ <link rel=\"stylesheet\" href=\"/css/app.css\" >\
                              \ <link href=\"https://fonts.googleapis.com/css?family=Anton|Gloria+Hallelujah|Raleway\" rel=\"stylesheet\">\
                              \ </head> \
                              \ <body> \
                              \   <div id=\"app\"></div> \
                              \   <script src=\"http://localhost:3040/static/home.js\"></script> \
                              \ </body>\
                              \ </html> "]

          get "/home/:userLogin" $ do
            userLogin <- param "userLogin"
            html $ mconcat [ " <!doctype> \
                              \ <html> \
                              \ <head> \
                              \ <title>purescript-webpack-example</title> \
                              \ <meta name=\"viewport\" content=\"width=device-width\">\
                              \ <link rel=\"stylesheet\" href=\"/css/app.css\" >\
                              \ <link href=\"http://cdnjs.cloudflare.com/ajax/libs/fullcalendar/3.4.0/fullcalendar.min.css\" rel=\"stylesheet\">\
                              \ <link href=\"https://fonts.googleapis.com/css?family=Anton|Gloria+Hallelujah|Raleway\" rel=\"stylesheet\">\
                              \ </head> \
                              \ <body> \
                              \   <div id=\"app\"></div> \
                              \   <script> var _userLogin='", userLogin, "';</script> \
                              \   <script src=\"http://localhost:3040/static/user.js\"></script> \
                              \ </body>\
                              \ </html> "]

          post "/api/login" $ do
            loginForm <- jsonData
            session <- liftIO $ doLogin pool loginForm
            case session of
              Nothing -> status unauthorized401
              Just usession -> do
                setCookie "session" (sessionCookie secret usession)
                status ok200
                Web.Scotty.json usession

          post "/api/logout" $ do
              setCookie "session" ""
              status ok200

          get "/api/me" $ do
            s <- readSession secret
            case s of
              Left err -> jsonUnauthorized err
              Right s -> do
                status ok200
                Web.Scotty.json s

          get "/api/users" $ do
              users <- liftIO $ findAllUsers pool
              status ok200
              Web.Scotty.json (users :: [User])

          post "/api/users" $ do
              u <- jsonData
              successOrFailure <- liftIO $ createUser pool u
              case successOrFailure of
                Left err -> badRequest $ TL.unpack err
                Right u -> do
                    status created201
                    Web.Scotty.json (u :: User)

          put "/api/users/:login" $ do
              login <- param "login"
              checkUserAuth login $ \s -> do
                u <- jsonData
                successOrFailure <- liftIO $ createOrUpdateUser pool login u
                case successOrFailure of
                  Left err -> badRequest $ TL.unpack err
                  Right u -> do
                      status created201
                      Web.Scotty.json (u :: User)

          get "/users/:login" $ do
              login <- param "login"
              checkUserAuth login $ \s -> do
                mayBeUser <- liftIO $ findUserByLogin pool login
                case mayBeUser of
                  Nothing -> status notFound404
                  Just user -> do
                    status ok200
                    Web.Scotty.json (user :: User)

          delete "/api/users/:login" $ do
              login <- param "login"
              checkUserAuth login $ \s -> do
                mayBeUser <- liftIO $ deleteUser pool login
                status noContent204

          get "/api/users/:login/slots" $ do
              login <- param "login"
              checkUserAuth login $ \s -> do
                slots <- liftIO $ findSlotsByUserLogin pool login
                status ok200
                Web.Scotty.json (slots :: [Slot])

          post "/api/users/:login/slots" $ do
              login <- param "login"
              checkUserAuth login $ \s -> do
                s <- jsonData
                mayBeUser <- liftIO $ findUserByLogin pool login
                case mayBeUser of
                  Nothing -> badRequest "User not found"
                  Just (User id _ _) -> do
                    successOrFailure <- liftIO $ createSlot pool (updateSlot id s)
                    case successOrFailure of
                      Left err -> badRequest $ TL.unpack err
                      Right u -> do
                          status created201
                          Web.Scotty.json (u :: Slot)
          delete "/api/users/:login/slots/:id" $ do
            login <- param "login"
            checkUserAuth login $ \s -> do
              id <- param "id"
              case UUID.fromString id of
                Just uuid -> do
                  _ <- liftIO $ deleteSlot pool uuid
                  status noContent204
                Nothing -> badRequest "Error decoding slot id"

          get "/api/users/:login/slots/:id/_search" $ do
            login <- param "login"
            checkUserAuth login $ \s -> do
              id <- param "id"
              case UUID.fromString id of
                Just uuid -> do
                  slots <- liftIO $ searchSimilarSlot pool uuid
                  status ok200
                  Web.Scotty.json (slots :: [Slot])
                Nothing -> badRequest "Error decoding slot id"

          where checkUserAuth = checkAuth secret
                updateSlot uid s = s {userId = uid}
