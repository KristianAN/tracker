{-# LANGUAGE OverloadedStrings #-}

module Persistence.TrackerRepository (
    deleteProject,
    insertProject,
    RepositoryActionResult (..),
    selectProject,
    selectAllProjects,
    insertNewEntry,
    selectActiveTracking,
    updateActiveTracking,
    unsetActiveTracking,
) where

import Control.Exception
import Data.Functor
import Data.Text qualified as T
import Data.Time (LocalTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format
import Database.SQLite.Simple
import Models.Project (Project (..))

data RepositoryActionResult a
    = Success {value :: a}
    | Error {reason :: T.Text}
    deriving (Show, Eq)

-- Repository actions for Project

insertProject ::
    Connection -> -- sqlite database connection
    Project -> -- The project to insert
    IO (RepositoryActionResult ())
insertProject conn project = do
    result <-
        try
            ( execute
                conn
                "insert into project (name, external_id) values (?, ?)"
                ((T.unpack $ name project, externalId project <&> T.unpack) :: (String, Maybe String))
            ) ::
            IO (Either SomeException ())
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right _ -> pure $ Success ()

selectProjectFr :: Query
selectProjectFr = "select name, external_id from project"

selectProject ::
    Connection -> -- Sqlite database connection
    T.Text -> -- name of the project to select
    IO (RepositoryActionResult (Maybe Project))
selectProject conn name = do
    result <-
        try
            ( query
                conn
                (selectProjectFr <> " where name = ?")
                (Only name) ::
                IO [(T.Text, Maybe T.Text)]
            ) ::
            IO (Either SomeException [(T.Text, Maybe T.Text)])
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right rows -> case rows of
            (pName, extId) : _ ->
                pure $
                    Success $
                        Just Project{name = pName, externalId = extId}
            [] -> pure $ Success Nothing

selectAllProjects :: Connection -> IO (RepositoryActionResult [Project])
selectAllProjects conn = do
    result <-
        try
            ( query_ conn selectProjectFr :: IO [(T.Text, Maybe T.Text)]
            ) ::
            IO (Either SomeException [(T.Text, Maybe T.Text)])
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right rows ->
            let projects =
                    fmap
                        ( \(pName, extId) ->
                            Project{name = pName, externalId = extId}
                        )
                        rows
             in pure $ Success projects

deleteProject ::
    Connection -> -- sqlite database connection
    T.Text -> -- name of project to delete
    IO (RepositoryActionResult ())
deleteProject conn name = do
    result <-
        try
            ( execute
                conn
                "delete from project where name = ?"
                (Only name)
            ) ::
            IO (Either SomeException ())
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right _ -> pure $ Success ()

-- Repository actions for active_tracking

updateActiveTracking :: Connection -> T.Text -> IO (RepositoryActionResult ())
updateActiveTracking conn name = do
    result <-
        try
            ( execute conn "update active_tracking set current_active = ? where id = 0" (Only name)
            ) ::
            IO (Either SomeException ())
    case result of
        Left err -> pure $ Error $ T.pack $ show err
        Right _ -> pure $ Success ()

unsetActiveTracking :: Connection -> IO (RepositoryActionResult ())
unsetActiveTracking conn = do
    result <-
        try
            ( execute_ conn "update active_tracking set current_active = null where id = 0"
            ) ::
            IO (Either SomeException ())
    case result of
        Left err -> pure $ Error $ T.pack $ show err
        Right _ -> pure $ Success ()

selectActiveTracking :: Connection -> IO (RepositoryActionResult (Maybe Project))
selectActiveTracking conn = do
    result <-
        try
            ( query_ conn "select current_active from active_tracking where id = 0" :: IO [Only (Maybe T.Text)]
            ) ::
            IO (Either SomeException [Only (Maybe T.Text)])
    case result of
        Right value -> case value of
            [maybeActive] ->
                case fromOnly maybeActive of
                    Just active -> selectProject conn active
                    Nothing ->
                        pure $
                            Error "No current_active set in table active_tracking"
            _ -> pure $ Success Nothing
        Left err -> pure $ Error $ T.pack $ show err

-- Repository actions for TimeEntry

timeToString :: LocalTime -> String
timeToString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

timeFromString :: String -> LocalTime
timeFromString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

insertNewEntry ::
    Connection ->
    T.Text -> -- project name
    IO (RepositoryActionResult ())
insertNewEntry conn name = do
    currentTime <- getCurrentTime
    timeZone <- getCurrentTimeZone
    result <-
        try
            ( execute
                conn
                "insert into time_entry (project_name, start_time) values (?, ?)"
                (name, timeToString (utcToLocalTime timeZone currentTime))
            ) ::
            IO (Either SomeException ())
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right _ -> pure $ Success ()
