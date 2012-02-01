module EVAN.Documentation where

import Text.JSON (decode, JSON, JSON(..), Result(..))

data Docs = Docs
  { published :: String
  }

instance JSON Docs where
  readJSON v = Ok $ Docs "hi"
  showJSON = undefined

readDocs :: String -> Either Docs String
readDocs str = case decode str of
                Ok d -> Left d
                Error e -> Right e

