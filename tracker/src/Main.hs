module Main where

import Persistence.SetupDatabase (initDatabase)

main :: IO ()
main = initDatabase
