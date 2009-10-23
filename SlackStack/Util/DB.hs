module SlackStack.Util.DB (
    module Database.HDBC,
    sqlAsString, rowMaps, rowMaps', rowMap, rowList, rowLists
) where

import Database.HDBC
import qualified Data.Map as M

sqlAsString :: SqlValue -> String
sqlAsString value = fromSql value

prepareExec :: IConnection conn =>
    (Statement -> IO a)
    -> conn -> String -> [SqlValue] -> IO a
prepareExec f dbh query params = do
    sth <- prepare dbh query
    execute sth params
    f sth

rowMaps :: IConnection conn => conn ->
    String -> [SqlValue] -> IO [M.Map String SqlValue]
rowMaps = prepareExec fetchAllRowsMap

rowMaps' :: IConnection conn => conn ->
    String -> [SqlValue] -> IO [M.Map String SqlValue]
rowMaps' = prepareExec fetchAllRowsMap'

rowMap :: IConnection conn => conn ->
    String -> [SqlValue] -> IO (Maybe (M.Map String SqlValue))
rowMap = prepareExec fetchRowMap

rowList :: IConnection conn => conn ->
    String -> [SqlValue] -> IO (Maybe [SqlValue])
rowList = prepareExec fetchRow

rowLists :: IConnection conn => conn ->
    String -> [SqlValue] -> IO [[SqlValue]]
rowLists = prepareExec fetchAllRows
