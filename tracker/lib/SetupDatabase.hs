{-# LANGUAGE OverloadedStrings #-}

module SetupDatabase (initDatabase, initializeSqlite) where

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

initializeSqlite :: Connection -> IO ()
initializeSqlite conn = do
    execute_ conn "PRAGMA foreign_keys = ON;"
    execute_ conn "create table if not exists project (name text primary key, external_id text)"
    execute_ conn "create table if not exists time_entry (project_name text not null, start_time text not null, end_time text)"

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
