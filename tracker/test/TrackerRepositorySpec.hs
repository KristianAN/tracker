{-# LANGUAGE OverloadedStrings #-}

module TrackerRepositorySpec (spec) where

import Control.Exception (bracket, evaluate)
import Database.SQLite.Simple
import Project (Project (..))
import SetupDatabase (initializeSqlite)
import Test.Hspec
import TrackerRepository (RepositoryActionResult (..), insertProject, selectProject)

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
