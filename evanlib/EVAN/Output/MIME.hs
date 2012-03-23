{-# LANGUAGE FlexibleInstances #-}

module EVAN.Output.MIME where

import Text.JSON (JSON(..), toJSObject)

data MIMEBox =
    Inline String String String
  | External String String String

class MIME a where
  mimebox :: a -> MIMEBox

instance JSON MIMEBox where
  readJSON = undefined
  showJSON (Inline t k d) = showJSON $ toJSObject
    [ ("mime", t)
    , ("kind", k)
    , ("data", d)
    ]
  showJSON (External t k f) = showJSON $ toJSObject
    [ ("mime", t)
    , ("kind", k)
    , ("file", f)
    ]

