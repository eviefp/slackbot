{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           CommandParser                (Command (..), parseCommand)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Either   (EitherT)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text, pack, replace, stripPrefix,
                                               unpack)
import           Data.Text.Lazy               (toStrict)
import           Data.Text.Lazy.Builder       (toLazyText)
import           HTMLEntities.Decoder         (htmlEncodedText)
import           Language.Haskell.Interpreter (eval, runInterpreter, setImports,
                                               typeOf)
import           System.Environment           (lookupEnv)
import           Web.Slack                    (Event (..), SlackBot,
                                               SlackConfig (..), runBot)
import           Web.Slack.Message            (sendMessage)

someFunc :: IO ()
someFunc = do
    slackConfig <- envMkSlackConfig "SLACK_API_TOKEN"
    --let config = SlackConfig { _slackApiToken = "xoxb-267381093382-X4yMFh37nbeuuP1XsBev73LV" }
    runBot slackConfig echoBot ()

envMkSlackConfig :: String -> IO SlackConfig
envMkSlackConfig key
     =  mkSlackConfig
    <$> fromMaybe (error $ key <> " not set")
    <$> lookupEnv key

mkSlackConfig :: String -> SlackConfig
mkSlackConfig apiToken = SlackConfig { _slackApiToken = apiToken }

formatReply :: String -> Text
formatReply msg = pack $ "```" ++ msg ++ "```"

evalMessage :: String -> IO (Either String String)
evalMessage msg = do
    res <- runInterpreter $ setImports ["Prelude"] >> eval msg
    case res of
        Left err -> return $ Left $ show err
        Right r  -> return $ Right r

typeMessage :: String -> IO (Either String String)
typeMessage msg = do
    res <- runInterpreter $ setImports ["Prelude"] >> typeOf msg
    case res of
        Left err -> return $ Left $ show err
        Right r  -> return $ Right r

evaluateCommand :: Command -> IO (Either String String)
evaluateCommand (Eval t) = evalMessage . unpack $ t
evaluateCommand (Type t) = typeMessage . unpack $ t

echoBot :: SlackBot a
echoBot (Message cid _ originalMsg _ _ _) = do
    let encodedMessage = toStrict . toLazyText . htmlEncodedText $ originalMsg
    let command = parseCommand encodedMessage
    case command of
        Left err  -> return ()
        Right cmd -> do
            res <- liftIO $ evaluateCommand cmd
            case res of
                Left err  -> sendMessage cid $ formatReply err
                Right txt -> sendMessage cid $ formatReply txt
            return ()
echoBot _ = return ()
