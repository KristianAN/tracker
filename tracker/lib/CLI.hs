module CLI (runCli) where

import Options.Applicative

data CLI = Start | Add deriving (Show)

cli :: Parser CLI
cli =
    hsubparser
        (command "start" (info (pure Start) (progDesc "Start time tracking")))

cliInfo :: ParserInfo CLI
cliInfo = info cli (fullDesc <> progDesc "Tracker, track your time" <> header "Tracker lets you track the time you spend working on your projects")

runCli :: IO CLI
runCli = execParser cliInfo
