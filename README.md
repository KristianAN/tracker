# Tracker

No, I don't want to track you!

# About

Tracker is a simple CLI application used for time tracking project work. Developed by me for my use-case.

# Usage

Tracker lets you add projects through the CLI or load a list of projects from file. Once you have projects you want to track you can start tracking by running 
```shell
tracker start.
```
tracker start with no arguments will try to match the current directory name with a project name, if it matches it starts a work entry. You can specify a specific project by running 
```shell
tracker start --name <name>
```
By design tracker only tracks one project at a time. If you have joined projects the reccomendation is to make a uniqe project for these. This constraint means that whenver tracker start is run tracker will stop tracking whatever project it is tracking before starting tracking of a new project.

to stop time tracking run
```shell
tracker stop
```
If tracker is not tracking anything when tracker stop is ran nothing happens.

Tracker is also build with data export in mind. For all export options run 
```shell
tracker export -h
```

For more usage information run 
```shell
tracker -h
```
# Technology and design

Tracker is built with simplicity in mind. The application is built using Haskell with a minimal set of libraries, notably sqlite-simple for persistence and optparse-applicative for structuring the CLI. 

Tracker stores all data in a sqlite database located at ~/.local/state/tracker/tracker_db.db

Tracker has high test coverage and uses hspec for testing.

