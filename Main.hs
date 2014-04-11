{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.List (intersperse)
import Data.Time
import Data.DateTime
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Console.CmdLib (Attributes(..), RecordCommand(..), Typeable, Data,
                              Attribute(..), (%>), group, getArgs,
                              recordCommands, dispatchR)
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
              | ShowPodcasts { numberToShow :: Int }
                deriving (Typeable, Data, Eq, Show)

instance Attributes Commands where
    attributes _ = group "Noname" [
      group "Adding a podcast" [
         number %> [ Help "Episode number", Default (1 :: Integer) ],
         guests %> Help "Guests of the episode",
         topics %> Help "Topics of the episode",
         start %> [ Help "Start date and time"
                  , Default "2014-01-01 00:00:00.0" ]
         ],
      group "Showing the podcasts" [
        numberToShow %> Help "How much podcasts to display"
        ]
      ]

instance RecordCommand Commands where
  run' cmd@(AddPodcast {}) _ = savePodcast cmd
  run' cmd@(ShowPodcasts {}) _ = showPodcasts cmd
  mode_summary (AddPodcast {}) = "add a podcast"
  mode_summary (ShowPodcasts {}) = "show podcasts"

csv :: String -> [String]
csv s = case dropWhile isComma s of
  "" -> []
  s' -> w : csv s''
    where (w, s'') =
            break isComma s'
  where isComma = (== ',')

main :: IO ()
main = do
  d <- doesFileExist databaseFilePath
  when (not d) initDatabase
  args <- getArgs
  cmd <- dispatchR [] args :: IO Commands
  run' cmd args

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

showPodcasts :: Commands -> IO ()
showPodcasts _ = do
  conn <- connectSqlite3 databaseFilePath
  rows <- quickQuery conn ("SELECT number, topics, guests, start_ " ++
                           " FROM podcasts") []
  mapM_ showPodcast rows
  disconnect conn
  return ()

showPodcast :: [SqlValue] -> IO ()
showPodcast values = do
  let number : topics : guests : start_ : [] = values
  putStrLn $ "Episode " ++ show (fromSql number :: Int)
  putStrLn $ "Topics:\n  " ++ (
    concat $ intersperse "\n  " (csv $ (fromSql topics :: String)))
  putStrLn $ "Guests:\n  " ++ (
    concat $ intersperse ", " (csv $ (fromSql guests :: String)))
  putStrLn $ "Start: " ++ show (fromSql start_ :: UTCTime)
  putStrLn ""
