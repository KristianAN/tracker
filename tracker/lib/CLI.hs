{-# LANGUAGE OverloadedStrings #-}

module CLI (runCli, CLI (..)) where

import Data.Text qualified as T
import Models.Project qualified as P
import Options.Applicative

data CLI = Start {project :: Maybe T.Text} | AddProject P.Project | List deriving (Show)

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

listParser :: Parser CLI
listParser = pure List

cli :: Parser CLI
cli =
    hsubparser
        ( command "start" (info startParser (progDesc "Start time tracking"))
            <> command "add" (info addProjectParser (progDesc "Add a project for tracking"))
            <> command "list" (info listParser (progDesc "List all projects"))
        )

cliInfo :: ParserInfo CLI
cliInfo = info (cli <**> helper) (fullDesc <> header "Tracker lets you track the time you spend working on your projects")

runCli :: IO CLI
runCli =
    customExecParser p opts
  where
    opts = cliInfo
    p = prefs showHelpOnEmpty
