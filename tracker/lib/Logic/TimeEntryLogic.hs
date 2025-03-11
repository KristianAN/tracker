{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.TimeEntryLogic (newTimeEntry) where

import Data.Text qualified as T
import Database.SQLite.Simple
import Models.Project qualified as P
import Persistence.TrackerRepository

{- | When a new entry is created we make sure that we stop any currently
| running entry before we create the new entry
-}
newTimeEntry ::
    Connection -> -- sqlite connection
    T.Text -> -- name of the project to create an entry for
    IO () -- Some IO
newTimeEntry conn name = do
    selectActiveTracking conn >>= \case
        Error err -> putStrLn $ T.unpack err
        Success maybeRunning ->
            case maybeRunning of
                Just running ->
                    let runningName = P.name running
                     in finalizeTimeEntry conn runningName >>= \case
                            Error err -> putStrLn $ T.unpack err
                            Success () -> putStrLn $ "Finalized time entry for " <> T.unpack runningName <> "."
                Nothing -> pure ()
    updateActiveTracking conn name >>= \case
        Error err -> putStrLn $ T.unpack err
        Success () ->
            insertNewEntry conn name >>= \case
                Error err -> putStrLn $ T.unpack err
                Success () -> putStrLn $ "Using provided project name. Started tracking time for project: " <> T.unpack name
