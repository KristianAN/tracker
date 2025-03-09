module Interpreter (interpret) where

import CLI
import Data.Text qualified as T
import Persistence.DatabaseUtils (withTrackerConnection)
import Persistence.TrackerRepository

interpret :: IO ()
interpret = do
    opts <- runCli
    case opts of
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
