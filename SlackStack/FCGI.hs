module Main where

import SlackStack.Handlers (handlers)

import Happstack.Server
import Happstack.Server.FastCGI
import Database.HDBC.Sqlite3 (connectSqlite3)

import System.Environment (getEnv,getProgName)
import Data.List (isSuffixOf)

main :: IO ()
main = do
    root <- getEnv "SLACKSTACK_ROOT"
    dbh <- connectSqlite3 (root ++ "/" ++ "substack.db")
    name <- getProgName
    let handler = handlers root dbh
    runFastCGIConcurrent 5 $ serverPartToCGI handler
