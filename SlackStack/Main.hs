module Main where

import SlackStack.Handlers (handlers)

import Happstack.Server
import Database.HDBC.Sqlite3 (connectSqlite3)

import System.Environment (getEnv,getProgName)
import Data.List (isSuffixOf)
import Control.Monad (mplus)

main :: IO ()
main = do
    root <- getEnv "SLACKSTACK_ROOT" `mplus` return "."
    dbh <- connectSqlite3 (root ++ "/" ++ "substack.db")
    name <- getProgName
    let handler = handlers root dbh
    simpleHTTP (Conf 5050 Nothing) handler
