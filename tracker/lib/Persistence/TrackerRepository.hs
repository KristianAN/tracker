{-# LANGUAGE OverloadedStrings #-}

module Persistence.TrackerRepository (
    deleteProject,
    insertProject,
    RepositoryActionResult (..),
    selectProject,
    selectAllProjects,
    insertNewEntry,
) where

import Control.Exception
import Data.Functor
import Data.Text qualified as T
import Data.Time (LocalTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format
import Database.SQLite.Simple
import Project (Project (..))

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

-- Repository actions for TimeEntry

timeToString :: LocalTime -> String
timeToString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

timeFromString :: String -> LocalTime
timeFromString timeString = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" timeString

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
