{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.TimeEntryLogicSpec (spec) where

import Data.Maybe
import Data.Text qualified as T
import Database.SQLite.Simple
import Logic.TimeEntryLogic (newTimeEntry)
import Models.Project (Project (..))
import Models.TimeEntry (TimeEntry (endtime))
import Persistence.DatabaseUtils (initializeSqlite)
import Persistence.TrackerRepository
import Test.Hspec
import TestUtils

exampleProject = Project{name = "name", externalId = Just "id"}
exampleProjectTwo = Project{name = "nameTwo", externalId = Just "id"}
exampleProjectNoExt = Project{name = "name", externalId = Nothing}

spec :: Spec
spec = do
    around withDatabase $ do
        describe "TimeEntryLogic Operations" $ do
            -- Tests for project ops
            it "Creating new time entry sets active tracking to correct project" $ \c -> do
                insertProject c exampleProject
                newTimeEntry c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProject)

            it "Starting second entry finalizes running entry" $ \c -> do
                insertProject c exampleProject
                insertProject c exampleProjectTwo
                newTimeEntry c "name"
                newTimeEntry c "nameTwo"
                selectTimeEntries c "name" >>= \case
                    Success res -> isJust (endtime (head res)) `shouldBe` True
                    Error err -> expectationFailure $ T.unpack err

            it "Starting second entry of same project finalizes running entry" $ \c -> do
                insertProject c exampleProject
                insertProject c exampleProjectTwo
                newTimeEntry c "name"
                newTimeEntry c "name"
                selectTimeEntries c "name" >>= \case
                    Success res -> isJust (endtime (head res)) `shouldBe` True
                    Error err -> expectationFailure $ T.unpack err

            it "Starting second entry finalizes second entry is running" $ \c -> do
                insertProject c exampleProject
                insertProject c exampleProjectTwo
                newTimeEntry c "name"
                newTimeEntry c "nameTwo"
                selectTimeEntries c "nameTwo" >>= \case
                    Success res -> isJust (endtime (head res)) `shouldBe` False
                    Error err -> expectationFailure $ T.unpack err

            it "Creating time entry for non-existent project returns appropriate error" $ \c -> do
                newTimeEntry c "nonexistent"
                selectActiveTracking c `shouldReturn` Success Nothing

            it "Creating time entry with project having no externalId works correctly" $ \c -> do
                insertProject c exampleProjectNoExt
                newTimeEntry c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProjectNoExt)
