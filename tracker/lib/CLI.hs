{-# LANGUAGE OverloadedStrings #-}

module CLI (runCli, CLI (..)) where

import Data.Text qualified as T
import Options.Applicative
import Project qualified as P

data CLI = Start {project :: Maybe T.Text} | AddProject P.Project deriving (Show)

startParser :: Parser CLI
startParser =
    Start
        <$> optional
            ( strOption
                (long "project" <> short 'p' <> help "Optional project name to override current directory name")
            )

addProjectParser :: Parser CLI
addProjectParser =
    AddProject
        <$> ( P.Project
                <$> strOption
                    ( long "name" <> short 'n' <> help "Name of the project to add"
                    )
                <*> optional
                    ( strOption
                        (long "externalId" <> short 'e' <> help "optional reference to external system")
                    )
            )

cli :: Parser CLI
cli =
    hsubparser
        ( command "start" (info startParser (progDesc "Start time tracking"))
            <> command "add" (info addProjectParser (progDesc "Add a project for tracking"))
        )

cliInfo :: ParserInfo CLI
cliInfo = info (cli <**> helper) (fullDesc <> header "Tracker lets you track the time you spend working on your projects")

runCli :: IO CLI
runCli =
    customExecParser p opts
  where
    opts = cliInfo
    p = prefs showHelpOnEmpty
