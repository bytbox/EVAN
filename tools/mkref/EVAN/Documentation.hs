module EVAN.Documentation where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.JSON

data Docs = Docs
  { generated   :: String
  , categories  :: Map String [Function]
  }

data Function = Function
  {
  }
  deriving Show

instance JSON Docs where
  readJSON (JSObject jso) = do
    JSString generatedStr <- lkup "generated"
    categoriesJSON <- lkup "categories"
    categories <- readJSON categoriesJSON
    return $ Docs
      { generated = fromJSString generatedStr
      , categories = Map.fromList categories
      }
    where
      obj = Map.fromList $ fromJSObject jso
      lkup k = case Map.lookup k obj of
                Just v -> Ok v
                Nothing -> Error ("key not found: " ++ k)
  showJSON = undefined

instance JSON Function where
  readJSON = const $ Ok Function
  showJSON = undefined

readDocs :: String -> Either Docs String
readDocs str = case decode str of
                Ok d -> Left d
                Error e -> Right e

