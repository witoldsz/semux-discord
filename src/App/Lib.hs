module App.Lib where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (log)
import qualified Data.ByteString.Lazy as BSL
import Control.Concurrent.MVar

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

logText :: Text -> IO ()
logText =
  logStr . unpack

logStr :: String -> IO ()
logStr =
  putStrLn

logJson :: (ToJSON a) => a -> IO ()
logJson =
  logText . prettyJson

type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock :: IO a -> Lock -> IO a
withLock io lock = withMVar lock (const io)

-- | Escape Discord formatting tags
-- TODO
escDF :: Text -> Text
escDF = id
--   (\c -> (c, "\\" <> c)) <$> special
--   replace "`" "\\`"
--   where
--     special :: [Text]
--     special = ["`", "*", "_", ":"]