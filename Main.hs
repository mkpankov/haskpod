{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Time
import Data.DateTime
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Console.CmdLib (Attributes(..), RecordCommand(..), Typeable, Data,
                              Attribute(..), (%>), group, getArgs,
                              dispatchR)
import System.Directory
import System.Locale

data Podcast = Podcast { episodeNumber_ :: Int
                       , guests_ :: [String]
                       , topics_ :: [String]
                       , start_ :: UTCTime
                       }
               deriving (Show)

data Commands = AddPodcast { number :: Int
                           , guests :: String
                           , topics :: String
                           , start :: String }
                deriving (Typeable, Data, Eq, Show)

instance Attributes Commands where
    attributes _ = group "Adding a podcast" [
        number %> [ Help "Episode number", Default (1 :: Integer) ],
        guests %> Help "Guests of the episode",
        topics %> Help "Topics of the episode",
        start %> [ Help "Start date and time"
                 , Default "01/01/14 00:00" ]
        ]


instance RecordCommand Commands where
  run' _ _ = undefined
  mode_summary _ = "add a podcast"

csv :: String -> [String]
csv s = case dropWhile isComma s of
  "" -> []
  s' -> w : csv s''
    where (w, s'') =
            break isComma s'
  where isComma = (== ',')

main = getArgs >>=
       ((dispatchR []) :: [String] -> IO Commands) >>=
       \cmds -> do
  d <- doesFileExist databaseFilePath
  when (not d) initDatabase
  case cmds of
    AddPodcast {} -> savePodcast cmds

savePodcast :: Commands -> IO ()
savePodcast p = do
  conn <- connectSqlite3 databaseFilePath
  print p
  run conn "INSERT INTO podcasts VALUES (NULL, ?, ?, ?, ?)"
    [toSql $ number p, toSql $ topics p,
     toSql $ guests p, toSql $ start p]
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
