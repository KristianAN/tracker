module Interpreter (interpret) where

import CLI
import Data.Text qualified as T
import Persistence.DatabaseUtils (withTrackerConnection)
import Persistence.TrackerRepository
import Project (prettyPrintProject)

interpret :: IO ()
interpret = do
    opts <- runCli
    case opts of
        List ->
            withTrackerConnection $ \c -> do
                result <- selectAllProjects c
                case result of
                    Success projects
                        | not (null projects) ->
                            let prettyPrinted = fmap prettyPrintProject projects
                                projectStr = T.intercalate (T.pack "\n") prettyPrinted
                             in putStrLn $ T.unpack projectStr
                        | otherwise ->
                            putStrLn
                                "You have not added any projects yet, consider adding a project with tracker add"
                    Error reason -> putStrLn $ T.unpack reason
        Start project ->
            case project of
                Just project -> undefined
                Nothing -> undefined
        AddProject project ->
            withTrackerConnection $ \c -> do
                result <- insertProject c project
                case result of
                    Success () -> putStrLn "Added project"
                    Error reason -> putStrLn $ T.unpack reason
