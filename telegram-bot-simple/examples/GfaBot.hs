{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)

import Telegram.Bot.API
import Telegram.Bot.Simple
-- import Telegram.Bot.Simple.UpdateParser

type Model = ()

data Action = Advice Text
  
data AdviceResponse = AdviceResponse
  { adviceResponseId    :: Int
  , adviceResponseText  :: Text
  , adviceResponseSound :: Text
  } deriving (Generic, Show)

instance FromJSON AdviceResponse where
  parseJSON = withObject "AdviceResponse" $ \v -> AdviceResponse
    <$> v .: "id"
    <*> v .: "text"
    <*> v .: "sound"

adviceURL :: String
adviceURL = "https://fucking-great-advice.ru/api/random"

gfaBot :: BotApp Model Action
gfaBot = BotApp
  { botInitialModel = ()
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update = Just (Advice "fucking monad")

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Advice a -> model <# do
    replyText $ a

getAdviceJSON :: IO L.ByteString
getAdviceJSON = simpleHttp adviceURL

getAdviceText :: L.ByteString -> Text
getAdviceText j = case decode' j :: Maybe AdviceResponse of
  Just advice -> adviceResponseText advice
  _           -> "error parsing JSON"

-- main :: IO ()
-- main = do
--     adviceJSON <- getAdviceJSON
--     let advice = getAdviceText adviceJSON
--     TIO.putStr advice

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ gfaBot env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
