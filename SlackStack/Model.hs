module SlackStack.Model (
    module Database.HDBC,
    Connection,
    getHandle
) where

import Control.Exception
import Database.HDBC
import Database.HDBC.Sqlite3 (connectSqlite3,Connection)

getHandle :: IO Connection
getHandle = connectSqlite3 "substack.db"

{-
do
    state <- prepare c "INSERT INTO testtable values (?,?);"
    select <- prepare c "SELECT * FROM testtable;"
    execute state [toSql "muhmuh", toSql (40::Int)]
    execute select []
    result <- fetchAllRows select
    putStrLn $ show result
    commit c
    disconnect c
    return ()

catcher :: SqlError -> IO ()
catcher err = putStrLn $ "SQL Error: " ++ show err
-}
