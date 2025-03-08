{-# LANGUAGE OverloadedStrings #-}

module SetupDatabase (initDatabase) where

import Control.Exception (bracket)
import Database.SQLite.Simple
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnv)
import System.FilePath ((</>))

databasePath :: IO FilePath
databasePath = do
  home <- getEnv "HOME"
  let dbDir = home </> ".local" </> "state" </> "tracker"
  createDirectoryIfMissing True dbDir
  pure $ dbDir </> "tracker_db.db"

initializeSqliteDb :: FilePath -> IO ()
initializeSqliteDb dbPath = do
  bracket (open dbPath) close $ \conn -> do
    execute_ conn "PRAGMA foreign_keys = ON;"
    execute_ conn "create table if not exists project (name text primary key, external_id text)"
    execute_ conn "create table if not exists time_entry (project_name text not null, start_time text not null, end_time text)"

initDatabase :: IO ()
initDatabase = databasePath >>= initializeSqliteDb
