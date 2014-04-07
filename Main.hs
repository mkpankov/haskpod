{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Time
import Data.DateTime
import System.Console.CmdLib
import System.Locale

data Podcast = Podcast { episodeNumber_ :: Int
                       , guests_ :: [String]
                       , topics_ :: [String]
                       , start_ :: UTCTime
                       }
               deriving (Show)

data Main = Main { episodeNumber :: Int
                 , guests :: String
                 , topics :: String
                 , start :: String }
    deriving (Typeable, Data, Eq, Show)

instance Attributes Main where
    attributes _ = group "Options" [
        episodeNumber %> [ Help "Episode number", Default (1 :: Integer) ],
        guests %> Help "Guests of the episode",
        topics %> Help "Topics of the episode",
        start %> [ Help "Start date and time"
                 , Default (Data.DateTime.fromGregorian 2014 1 1 0 0 0) ]
        ]

instance RecordCommand Main where
    mode_summary _ = "Hello world with argument parsing."

csv :: String -> [String]
csv s = case dropWhile isComma s of
  "" -> []
  s' -> w : csv s''
    where (w, s'') =
            break isComma s'
  where isComma = (== ',')

main = getArgs >>= executeR Main {} >>= \opts -> do
  let ts = start opts
      Just t = (parseTime defaultTimeLocale "%D %R" ts) :: Maybe UTCTime
      gs = csv $ guests opts
      ps = csv $ topics opts
      p  = Podcast (episodeNumber opts) gs ps t
  print p
