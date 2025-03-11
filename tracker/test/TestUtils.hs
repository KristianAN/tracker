module TestUtils (withDatabase) where

import Persistence.DatabaseUtils

import Control.Exception
import Database.SQLite.Simple

openWithTables :: IO Connection
openWithTables = do
    conn <- open ":memory:"
    initializeSqlite conn
    pure conn

withDatabase :: (Connection -> IO ()) -> IO ()
withDatabase = bracket openWithTables close
