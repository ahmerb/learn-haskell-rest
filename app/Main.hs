{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics

-- Datatype person which can be encoded and decoded between ByteString
-- of serialized JSON and Maybe Person

data Person = Person
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

instance ToJSON Person

instance FromJSON Person

-- Datatype Plaintext represents a json { myText: "some text" }

data PlainText = PlainText { myText :: Text }
               deriving (Generic, Show)

instance ToJSON PlainText

instance FromJSON PlainText

-- basic Spock app to serve JSON

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get root $ json $ PlainText "this is the home page"
  get "person" $ do
    json $ Person { name = "Fry", age = 25 }
    -- the json function serves any type that implements ToJSON
    -- json will take a ToJSON a, serialize a as JSON and set content-type to application/json
  get "people" $ do
    json [ Person { name =    "Fry", age = 25 }
         , Person { name = "Bender", age =  4 }
         ]
  post "person" $ do
    thePerson <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show thePerson)
  