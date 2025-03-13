{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.TimeEntryLogic (newTimeEntry, stopTimeEntry) where

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
    IO (Maybe T.Text, T.Text) -- Some IO
newTimeEntry conn name = do
    selectOut <- stopTimeEntry conn True
    updateOut <-
        updateActiveTracking conn name >>= \case
            Error err -> pure err
            Success () ->
                insertNewEntry conn name >>= \case
                    Error err -> pure err
                    Success () -> pure $ "Using provided project name. Started tracking time for project: " <> name

    pure (selectOut, updateOut)

stopTimeEntry :: Connection -> Bool -> IO (Maybe T.Text)
stopTimeEntry conn fromNew = do
    selectActiveTracking conn >>= \case
        Error err -> pure $ Just err
        Success maybeRunning ->
            case maybeRunning of
                Just running ->
                    let runningName = P.name running
                     in finalizeTimeEntry conn runningName >>= \case
                            Error err -> pure $ Just err
                            Success () -> pure $ Just $ "Finalized time entry for " <> runningName <> "."
                Nothing -> pure $ if not fromNew then Just "Nothing to stop. No time is being tracked" else Nothing
