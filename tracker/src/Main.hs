module Main where

import CLI (runCli)
import Persistence.DatabaseUtils (initDatabase)

main :: IO ()
main = do
    initDatabase
    options <- runCli
    print options
