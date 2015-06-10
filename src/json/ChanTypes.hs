{-# LANGUAGE OverloadedStrings, DeriveGeneric, BangPatterns #-}

module ChanTypes where

import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text as T
import GHC.Generics


data Page =
  Page {
          page :: Int               -- pagenumber
       ,  threads :: [ThreadInfo]   -- threads on that page
       }  deriving (Show, Generic)

data ThreadInfo =
  ThreadInfo {
           no :: Integer            -- thread number
         , last_modified :: Integer -- in s since 1970
         }  deriving (Show, Generic)

data Thread =
  Thread {
          posts :: [Post]           -- posts in that thread
        } deriving (Show, Generic)

data Post =
  Post {
        --sub :: T.Text              -- headline
         com :: T.Text              -- contents
       --, time :: Integer
       } deriving (Show, Generic)

instance FromJSON Page
instance ToJSON Page

instance FromJSON ThreadInfo
instance ToJSON ThreadInfo

instance FromJSON Thread
instance ToJSON Thread

instance FromJSON Post
instance ToJSON Post




--instance FromJSON Page where
--  parseJSON (Object v) =
--    Page <$> v .: "page"
--         <*> v .: "threads"
--  parseJSON _          = mzero

--instance ToJSON Page where
-- toJSON (Page page threads ) =
--    object [ "page" .= page
--           , "threads" .= threads
--           ]