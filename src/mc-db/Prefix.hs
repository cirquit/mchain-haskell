-- @Prefix.hs
{-# LANGUAGE TemplateHaskell #-}
module Prefix where

import Data.Text.Lazy (Text)
import Database.Persist.TH

data Prefix = Prefix {pre :: Text, count :: Int}
  deriving (Show, Read, Eq)
derivePersistField "Prefix"