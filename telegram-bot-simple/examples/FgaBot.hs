{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (isJust)


import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = ()

data Action = Start | Advice
  
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

fgaBot :: BotApp Model Action
fgaBot = BotApp
  { botInitialModel = ()
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update 
  | isJust $ parseUpdate (command "start") update = Just Start
  | isJust $ parseUpdate (command "advice") update = Just Advice
  | isJust $ parseUpdate (commandWithBotName "hsssss_bot" "advice") update = Just Advice
  | otherwise = Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  Advice -> model <# do
    adviceJSON <- liftIO getAdviceJSON
    let advice = getAdviceText adviceJSON
    replyText $ advice
  Start -> model <# do
    replyText $ startMessage where
      startMessage = Text.unlines
            [ "Привет, я бот, который умеет давать рандомные охуенные советы."
            , ""
            , "Чтобы получить совет, воспользуйтесь командой /advice."
            ]

getAdviceJSON :: IO L.ByteString
getAdviceJSON = simpleHttp adviceURL

getAdviceText :: L.ByteString -> Text
getAdviceText j = case decode' j :: Maybe AdviceResponse of
  Just advice -> adviceResponseText advice
  _           -> "error parsing JSON"

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ fgaBot env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
