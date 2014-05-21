{--------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2003
 wxWindows License.

 A database browser in wxHaskell.
 Demonstrates: 
 - list control, list boxes, and splitter windows
 - database access
--------------------------------------------------------------------------------}
module Main where

import List( intersperse )
import System.IO.Unsafe( unsafePerformIO )
import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main  
  = start $
    do f <- frame [text := "Database browser"]
       p <- panel f []

       s1      <- splitterWindow p []
       dsns    <- dbGetDataSources
       dsnList <- singleListBox s1 
                    [tooltip    := "Data sources"
                    ,items      := (reverse (map fst dsns))]

       s2        <- splitterWindow s1 []
       tableList <- singleListBox s2 [tooltip := "Tables"]
       tableView <- listCtrl s2 []

       set f [layout := container p $ margin 5 $ fill $ 
                        hsplit s1 5 80 (widget dsnList) 
                                       (hsplit s2 5 80 (widget tableList) (widget tableView))
             ,clientSize := sz 300 300
             ]
       
       set dsnList   [on select := onDsnEvent   f dsnList tableList tableView]
       set tableList [on select := onTableEvent f dsnList tableList tableView]
   where
      -- data source name selected: show tables in the tableList
      onDsnEvent f dsnList tableList tableView
        = do set tableView [items := [], columns := []]            
             set tableList [items := []]
             i    <- get dsnList selection
             dsn  <- get dsnList (item i)
             info <- dbGetDataSourceInfo dsn "" "" 
             set tableList [items := map tableName (reverse (dbTables info))]
          `catchDbError` \err -> errorDialog f ("Database '" ++ dbDataSource err ++ "'") (dbErrorMsg err)

      -- table selected: show contents in the tableView
      onTableEvent f dsnList tableList tableView
        = do set tableView [items := []]
             dsnIdx   <- get dsnList selection
             dsn      <- get dsnList (item dsnIdx)
             tableIdx <- get tableList selection
             tname    <- get tableList (item tableIdx)
             tableInfo <- dbGetDataSourceTableInfo dsn tname "" ""

             let columnInfos = tableColumns tableInfo             
             let headers     = map headerFromColumn columnInfos

             set tableView [columns := headers]
             dbWithConnection dsn "" "" $ \db ->
              do elems <- dbQuery db ("SELECT * FROM " ++ toSqlTableName db tname) 
                           (\row -> mapM (\i -> dbRowGetString row (show i)) (map columnIndex columnInfos))
                 set tableView [items := elems]
             return ()
          `catchDbError` \err -> 
             errorDialog f ("Database '" ++ dbDataSource err ++ "'")
                (case dbErrorCode err of
                   DB_ERR_WRONG_NO_OF_PARAMS 
                         -> "This is a table view function and can not be displayed.\n(" ++ dbErrorMsg err ++ ")"
                   other -> dbErrorMsg err)
        where
          headerFromColumn :: ColumnInfo -> (String,Align,Int)
          headerFromColumn info
            = (columnName info, AlignLeft, 10 + 6*headerWidth (max (columnSize info) (length (columnName info))))
    
          headerWidth n
            | n <= 2    = 2
            | n  > 15   = 15
            | otherwise = n
             

