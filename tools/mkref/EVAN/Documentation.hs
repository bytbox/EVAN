module EVAN.Documentation where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.JSON

readJSON' :: (Ord a, JSKey a, JSON b) => JSValue -> Result (Map a b)
readJSON' o = do
  m <- decJSDict "Map" o
  return $ Map.fromList m

data Docs = Docs
  { generated   :: String
  , categories  :: Map String Category
  }

data Category = Category
  { title       :: String
  , description :: String
  , functions   :: [Function]
  }
  deriving Show

data Function = Function
  { name      :: String
  , signature :: String
  , docstring :: String
  }
  deriving Show

instance JSON Docs where
  readJSON (JSObject jso) = do
    JSString generatedStr <- lkup "generated"
    categoriesJSON <- lkup "categories"
    categories <- readJSON' categoriesJSON
    return $ Docs
      { generated = fromJSString generatedStr
      , categories = categories
      }
    where
      obj = Map.fromList $ fromJSObject jso
      lkup k = case Map.lookup k obj of
                Just v -> Ok v
                Nothing -> Error ("key not found: " ++ k)
  showJSON = undefined

instance JSON Category where
  readJSON (JSObject jso) = do
    functionsJSON <- lkup "functions"
    functions <- readJSON functionsJSON
    titleJSON <- lkup "title"
    title <- readJSON titleJSON
    return $ Category
      { title       = title
      , functions   = functions
      , description = "DESC"
      }
    where
      obj = Map.fromList $ fromJSObject jso
      lkup k = case Map.lookup k obj of
                Just v -> Ok v
                Nothing -> Error ("key not found: " ++ k)
  showJSON = undefined

instance JSON Function where
  readJSON (JSObject jso) = do
    (JSString name) <- lkup "name"
    (JSString sig) <- lkup "sig"
    (JSString docs) <- lkup "docs"
    return $ Function
      { name = fromJSString name
      , signature = fromJSString sig
      , docstring = fromJSString docs
      }
    where
      obj = Map.fromList $ fromJSObject jso
      lkup k = case Map.lookup k obj of
                Just v -> Ok v
                Nothing -> Error ("key not found: " ++ k)
  showJSON = undefined

readDocs :: String -> Either Docs String
readDocs str = case decode str of
                Ok d -> Left d
                Error e -> Right e

