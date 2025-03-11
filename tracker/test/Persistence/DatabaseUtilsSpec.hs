{-# LANGUAGE OverloadedStrings #-}

module Persistence.DatabaseUtilsSpec (spec) where

import Control.Exception (bracket, evaluate)
import Database.SQLite.Simple
import Persistence.DatabaseUtils (initializeSqlite)
import Test.Hspec
import TestUtils

getTable :: Connection -> String -> IO [Only Int]
getTable conn tableName =
    query conn "SELECT count(*) FROM sqlite_master WHERE type= 'table' AND name= ?" (Only tableName) :: IO [Only Int]

spec :: Spec
spec = do
    around withDatabase $ do
        describe "Persistence.DatabaseUtils Operations" $ do
            it "project table is created" $ \c -> do
                getTable c "project" `shouldReturn` [Only 1]

            it "time_entry table is created" $ \c -> do
                getTable c "time_entry" `shouldReturn` [Only 1]

            it "active_tracking table is created" $ \c -> do
                getTable c "active_tracking" `shouldReturn` [Only 1]
