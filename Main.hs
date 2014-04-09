{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Time
import Data.DateTime
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Console.CmdLib (Attributes(..), RecordCommand(..), Typeable, Data,
                              Attribute(..), (%>), executeR, group, getArgs)
import System.Directory
import System.Locale

data Podcast = Podcast { episodeNumber_ :: Int
                       , guests_ :: [String]
                       , topics_ :: [String]
                       , start_ :: UTCTime
                       }
               deriving (Show)

data Haskpod = Haskpod { episodeNumber :: Int
                       , guests :: String
                       , topics :: String
                       , start :: String }
    deriving (Typeable, Data, Eq, Show)

instance Attributes Haskpod where
    attributes _ = group "Adding a podcast" [
        episodeNumber %> [ Help "Episode number", Default (1 :: Integer) ],
        guests %> Help "Guests of the episode",
        topics %> Help "Topics of the episode",
        start %> [ Help "Start date and time"
                 , Default "01/01/14 00:00" ]
        ]

instance RecordCommand Haskpod where
  run' = undefined
  mode_summary _ = "Podcast management tool."

csv :: String -> [String]
csv s = case dropWhile isComma s of
  "" -> []
  s' -> w : csv s''
    where (w, s'') =
            break isComma s'
  where isComma = (== ',')

main = getArgs >>= executeR (Haskpod 0 "" "" "") >>= \opts -> do
  let ts = start opts
      Just t = (parseTime defaultTimeLocale "%D %R" ts) :: Maybe UTCTime
      gs = csv $ guests opts
      ps = csv $ topics opts
      p  = Podcast (episodeNumber opts) gs ps t
  d <- doesFileExist databaseFilePath
  when (not d) initDatabase
  savePodcast p
  print p

savePodcast p = do
  conn <- connectSqlite3 databaseFilePath
  run conn "INSERT INTO podcasts VALUES (NULL, ?, ?, ?, ?)"
    [toSql $ episodeNumber_ p, toSql $ unlines $ topics_ p,
     toSql $ unlines $ guests_ p, toSql $ start_ p]
  commit conn
  disconnect conn
  return ()

initDatabase = do
  conn <- connectSqlite3 databaseFilePath
  run conn ("CREATE TABLE podcasts" ++
               " ( id     INTEGER PRIMARY KEY ASC " ++
               " , number INTEGER NOT NULL " ++
               " , topics TEXT NOT NULL " ++
               " , guests TEXT NOT NULL " ++
               " , start_ INTEGER NOT NULL ) ") []
  commit conn
  disconnect conn
  return ()

databaseFilePath = "podcasts.db"
