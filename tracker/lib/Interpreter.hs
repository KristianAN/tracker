module Interpreter (interpret) where

import CLI
import Data.Functor
import Data.Text qualified as T
import Database.SQLite.Simple
import Persistence.DatabaseUtils (withTrackerConnection)
import Persistence.TrackerRepository
import Project
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
startTracking project =
    case project of
        Just projectName ->
            withTrackerConnection $ \c -> do
                result <- insertNewEntry c projectName
                case result of
                    Success () -> putStrLn $ "Using provided project name. Started tracking time for project: " <> T.unpack projectName
                    Error reason -> putStrLn $ T.unpack reason
        Nothing -> do
            currentDir <- getCurrentDirectory <&> takeFileName
            withTrackerConnection $ \c -> do
                result <- insertNewEntry c $ T.pack currentDir
                case result of
                    Success () -> putStrLn $ "Started tracking time for project: " <> currentDir
                    Error reason -> putStrLn $ T.unpack reason

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
