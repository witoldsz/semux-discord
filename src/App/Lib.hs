module App.Lib where

import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (log)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as DTL

rightOrError :: (Monad m) => String -> m (Either String a) -> m a
rightOrError msg =
  (>>= either errorMsg return)
  where
    errorMsg left = error (msg ++ "\ncaused by: " ++ left)

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

logJ :: (ToJSON a) => a -> IO ()
logJ =
  logText . decodeUtf8 . BSL.toStrict . encodePretty
