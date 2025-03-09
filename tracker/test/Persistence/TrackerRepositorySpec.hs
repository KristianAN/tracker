{-# LANGUAGE OverloadedStrings #-}

module Persistence.TrackerRepositorySpec (spec) where

import Control.Exception (bracket, evaluate)
import Database.SQLite.Simple
import Persistence.DatabaseUtils (initializeSqlite)
import Persistence.TrackerRepository
import Project (Project (..))
import Test.Hspec

openWithTables :: IO Connection
openWithTables = do
    conn <- open ":memory:"
    initializeSqlite conn
    pure conn

withDatabase :: (Connection -> IO ()) -> IO ()
withDatabase = bracket openWithTables close

exampleProject = Project{name = "name", externalId = Just "id"}

exampleProjectNoExt = Project{name = "name", externalId = Nothing}

spec :: Spec
spec = do
    around withDatabase $ do
        describe "Repository Operations" $ do
            it "inserting a project succeeds" $ \c -> do
                insertProject c exampleProject `shouldReturn` Success ()

            it "inserting a project without externalId succeeds" $ \c -> do
                insertProject c exampleProjectNoExt `shouldReturn` Success ()

            it "reading a project succeeds" $ \c -> do
                insertProject c exampleProject
                selectProject c "name" `shouldReturn` Success (Just exampleProject)

            it "reading a project with wrong id gives Nothing" $ \c -> do
                insertProject c exampleProject
                selectProject c "noname" `shouldReturn` Success Nothing

            it "deleting project succeeds" $ \c -> do
                insertProject c exampleProject
                deleteProject c "name" `shouldReturn` Success ()

            it "deleting a non-existant project succeeds" $ \c -> do
                insertProject c exampleProject
                deleteProject c "noname" `shouldReturn` Success ()

            it "can list all projects" $ \c -> do
                insertProject c exampleProject
                let secondProject = exampleProject{name = "name_two"}
                insertProject c secondProject
                selectAllProjects c `shouldReturn` Success ([exampleProject, secondProject])

            it "can insert a new time entry" $ \c -> do
                insertProject c exampleProject

                insertNewEntry c "name" `shouldReturn` Success ()
