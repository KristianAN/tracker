module Interpreter (interpret) where

import CLI
import Data.Functor
import Data.Text qualified as T
import Database.SQLite.Simple
import Logic.TimeEntryLogic (newTimeEntry)
import Models.Project
import Persistence.DatabaseUtils (withTrackerConnection)
import Persistence.TrackerRepository
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

listAllProjects :: Connection -> IO ()
listAllProjects c = do
    result <- selectAllProjects c
    case result of
        Success projects
            | not (null projects) ->
                let prettyPrinted = fmap prettyPrintProject projects
                    projectStr = T.intercalate (T.pack "\n") prettyPrinted
                 in putStrLn $ T.unpack projectStr
            | otherwise ->
                putStrLn
                    "Nothing to list, consider adding a project with tracker add"
        Error reason -> putStrLn $ T.unpack reason

startTracking :: Maybe T.Text -> IO ()
startTracking project = do
    path <- case project of
        Nothing ->
            getCurrentDirectory <&> takeFileName
        Just projectName -> pure $ T.unpack projectName
    withTrackerConnection $ \c -> do
        newTimeEntry c $ T.pack path
addProject :: Project -> IO ()
addProject project =
    withTrackerConnection $ \c -> do
        result <- insertProject c project
        case result of
            Success () -> putStrLn "Added project"
            Error reason -> putStrLn $ T.unpack reason

interpret :: IO ()
interpret = do
    opts <- runCli
    case opts of
        List -> withTrackerConnection listAllProjects
        Start project -> startTracking project
        AddProject project -> addProject project
