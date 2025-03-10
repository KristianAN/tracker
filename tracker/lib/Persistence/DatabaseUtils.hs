{-# LANGUAGE OverloadedStrings #-}

module Persistence.DatabaseUtils (initDatabase, initializeSqlite, withTrackerConnection) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing, doesPathExist)
import System.Environment (getEnv)
import System.FilePath ((</>))

databasePath :: IO FilePath
databasePath = do
    home <- getEnv "HOME"
    let dbDir = home </> ".local" </> "state" </> "tracker"
    createDirectoryIfMissing True dbDir
    pure $ dbDir </> "tracker_db.db"

withTrackerConnection :: (Connection -> IO a) -> IO a
withTrackerConnection dbOp = do
    path <- databasePath
    bracket (open path) close dbOp

projectTableQuery :: Query
projectTableQuery =
    "create table if not exists project"
        <> "(name text primary key,"
        <> "external_id text)"

timeEntryTableQuery :: Query
timeEntryTableQuery =
    "create table if not exists time_entry"
        <> "(project_name text not null,"
        <> "start_time text not null,"
        <> "end_time text,"
        <> "foreign key (project_name) references project(name))"

activeTrackingQuery :: Query
activeTrackingQuery =
    "create table if not exists active_tracking"
        <> "(id integer primary key check (id = 0),"
        <> "current_active text)"

activeTrackingDefault :: Query
activeTrackingDefault =
    "insert into active_tracking (id, current_active) values(0, null)"

pragmas :: Connection -> IO ()
pragmas conn =
    execute_ conn "PRAGMA foreign_keys = ON;"

initializeSqlite :: Connection -> IO ()
initializeSqlite conn = do
    pragmas conn
    execute_ conn projectTableQuery
    execute_ conn timeEntryTableQuery
    execute_ conn activeTrackingQuery
    execute_ conn activeTrackingDefault

initializeSqliteDbWithPath :: FilePath -> IO ()
initializeSqliteDbWithPath dbPath = do
    bracket (open dbPath) close initializeSqlite

initDatabase :: IO ()
initDatabase = do
    path <- databasePath
    exists <- doesPathExist path
    unless exists $ do
        putStrLn $ "This is your first time using tracker. Welcome!\nInfo: Initializing new database at " <> path
    initializeSqliteDbWithPath path
