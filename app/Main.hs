{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get, delete) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Sqlite hiding (get, delete)
import           Database.Persist.TH

-- Datatype person which can be encoded and decoded between ByteString
-- of serialized JSON and Maybe Person

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
  name Text
  age Int
  deriving Show
|]

-- Datatype Plaintext represents a json { myText: "some text" }

data PlainText = PlainText { myText :: Text }
               deriving (Generic, Show)

instance ToJSON PlainText

instance FromJSON PlainText

-- basic Spock app to serve JSON

type Api = SpockM SqlBackend () () ()

type ApiAction a = SpockAction SqlBackend () () a

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

errorJson :: Int -> Text -> ApiAction ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error"  .= object ["code" .= code, "message" .= message]
    ]

app :: Api
app = do
  get root $ json $ PlainText "this is the home page"
  get ("people" <//> var) $ \personId -> do
    maybePerson <- runSQL $ P.get personId :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 2 "could not find person with matching id"
      Just thePerson -> json thePerson
  get "people" $ do
    allPeople <- runSQL $ selectList [] [Asc PersonId]
    json allPeople
  post "people" $ do
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing        -> errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        newId <- runSQL $ insert thePerson
        json $ object ["result" .= String "success", "id" .= newId]
  put ("people" <//> var) $ \personId -> do 
    maybePerson <- jsonBody :: ApiAction (Maybe Person)
    case maybePerson of
      Nothing -> errorJson 1 "Failed to parse request body as Person"
      Just thePerson -> do
        runSQL $ replace personId thePerson
        json $ object ["result" .= String "success", "id" .= personId]
  delete ("people" <//> var) $ \personId -> do
    runSQL $ P.delete (personId :: PersonId)
    json $ object ["result" .= String "success", "id" .= personId]
