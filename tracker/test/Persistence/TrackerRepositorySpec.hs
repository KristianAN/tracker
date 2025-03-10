{-# LANGUAGE OverloadedStrings #-}

module Persistence.TrackerRepositorySpec (spec) where

import Control.Exception (bracket, evaluate)
import Data.Text qualified as T
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
            -- Tests for project ops
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

            it "inserting a project with duplicate name fails" $ \c -> do
                insertProject c exampleProject
                result <- insertProject c exampleProject
                case result of
                    Error _ -> return ()
                    Success _ -> fail "Expected insert with duplicate name to fail"

            it "inserting a project with very long name succeeds" $ \c -> do
                let longNameProject = Project{name = T.replicate 1000 "a", externalId = Nothing}
                insertProject c longNameProject `shouldReturn` Success ()

            it "inserting and selecting a project with special characters succeeds" $ \c -> do
                let specialProject = Project{name = "project!@#$%^&*()", externalId = Just "ext!@#$%^&*()"}
                insertProject c specialProject
                selectProject c "project!@#$%^&*()" `shouldReturn` Success (Just specialProject)

            it "selectAllProjects returns empty list when no projects exist" $ \c -> do
                selectAllProjects c `shouldReturn` Success []

            -- Time entry ops tests

            it "can insert a new time entry" $ \c -> do
                insertProject c exampleProject

                insertNewEntry c "name" `shouldReturn` Success ()

            it "inserting multiple time entries for the same project succeeds" $ \c -> do
                insertProject c exampleProject
                insertNewEntry c "name" `shouldReturn` Success ()
                insertNewEntry c "name" `shouldReturn` Success ()

            -- Active tracking ops test

            it "can update active tracking" $ \c -> do
                insertProject c exampleProject
                updateActiveTracking c "name" `shouldReturn` Success ()

            it "select active tracking gives correct project" $ \c -> do
                insertProject c exampleProject
                updateActiveTracking c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProject)

            it "unset active tracking makes it impossible to select active" $ \c -> do
                insertProject c exampleProject
                updateActiveTracking c "name"
                unsetActiveTracking c "name"
                selectActiveTracking c `shouldReturn` Error "No current_active set in table active_tracking"

            it "updating active tracking to non-existent project still succeeds at database level" $ \c -> do
                updateActiveTracking c "non-existent-project" `shouldReturn` Success ()

            it "selecting active tracking after setting non-existent project returns Nothing" $ \c -> do
                updateActiveTracking c "non-existent-project"
                selectActiveTracking c `shouldReturn` Success Nothing

            it "changing active tracking from one project to another works" $ \c -> do
                let project1 = Project{name = "project1", externalId = Nothing}
                let project2 = Project{name = "project2", externalId = Nothing}
                insertProject c project1
                insertProject c project2
                updateActiveTracking c "project1"
                selectActiveTracking c `shouldReturn` Success (Just project1)
                updateActiveTracking c "project2"
                selectActiveTracking c `shouldReturn` Success (Just project2)
