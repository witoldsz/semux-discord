module App.Lib where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (log)
import qualified Data.ByteString.Lazy as BSL

rightOrError :: (Monad m) => String -> m (Either String a) -> m a
rightOrError msg =
  (>>= either errorMsg return)
  where
    errorMsg left = error (msg ++ "\ncaused by: " ++ left)

prettyJson :: (ToJSON a) => a -> Text
prettyJson =
  decodeUtf8 . BSL.toStrict . encodePretty

logEmptyLine :: IO ()
logEmptyLine =
  putStrLn ""

log :: (Show a) => a -> IO ()
log =
  logStr . show

logText :: Text -> IO ()
logText =
  logStr . unpack

logStr :: String -> IO ()
logStr =
  putStrLn . (++ "\n")

logJson :: (ToJSON a) => a -> IO ()
logJson =
  logText . prettyJson
