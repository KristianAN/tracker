module Main where

import Interpreter
import Persistence.DatabaseUtils (initDatabase)

main :: IO ()
main = do
    initDatabase
    interpret
