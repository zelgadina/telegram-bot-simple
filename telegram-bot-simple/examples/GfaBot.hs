{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Data.Aeson
import           Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as L
import           GHC.Generics

-- import Telegram.Bot.API
-- import Telegram.Bot.Simple
-- import Telegram.Bot.Simple.UpdateParser


-- type Model = ()

-- data Action
--   = Inline Text
--   | Advice Text

data Response = Response
  { responseId    :: Int
  , responseText  :: Text
  , responseSound :: Text
  } deriving (Generic, Show)

instance FromJSON Response where
    parseJSON = withObject "Response" $ \v -> Response
        <$> v .: "id"
        <*> v .: "text"
        <*> v .: "sound"

adviceURL :: String
adviceURL = "https://fucking-great-advice.ru/api/random"

getJSON :: IO L.ByteString
getJSON = simpleHttp adviceURL

getAdvice :: L.ByteString -> Text
getAdvice j = case decode' j :: Maybe Response of
    Just advice -> responseText advice
    _           -> "error parsing JSON"

main :: IO ()
main = do
    adviceJSON <- getJSON
    let advice = getAdvice adviceJSON
    TIO.putStr advice

-- bashBot :: BotApp Model Action
-- bashBot = BotApp
--   { botInitialModel = ()
--   , botAction = updateToAction
--   , botHandler = handleAction
--   , botJobs = []
--   }

-- updateToAction :: Update -> Model -> Maybe Action


-- handleAction :: Action -> Model -> Eff Action Model


-- run :: Token -> IO ()
-- run token = do
--   env <- defaultTelegramClientEnv token
--   startBot_ bashBot env

-- main :: IO ()
-- main = do
--   putStrLn "Please, enter Telegram bot's API token:"
--   token <- Token . Text.pack <$> getLine
--   run token
