{-# LANGUAGE FlexibleInstances #-}

module EVAN.Output.Mime where

import Text.JSON (JSON(..), toJSObject)

data MIMEBox = MIMEBox String String String

class MIME a where
  mimebox :: a -> MIMEBox

instance JSON MIMEBox where
  readJSON = undefined
  showJSON (MIMEBox t k d) = showJSON $ toJSObject
    [ ("mime", t)
    , ("kind", k)
    , ("data", d)
    ]

