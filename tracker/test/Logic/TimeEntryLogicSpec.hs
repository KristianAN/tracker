{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Logic.TimeEntryLogicSpec (spec) where

import Data.Maybe
import Data.Text qualified as T
import Database.SQLite.Simple
import Logic.TimeEntryLogic (newTimeEntry, stopTimeEntry)
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

            it "Starting many entries should have just one that is tracking" $ \c -> do
                insertProject c exampleProject
                newTimeEntry c "name"
                newTimeEntry c "name"
                newTimeEntry c "name"
                newTimeEntry c "name"
                selectTimeEntries c "name" >>= \case
                    Success res -> do
                        length (filter (isNothing . endtime) res) `shouldBe` 1
                        length (filter (isJust . endtime) res) `shouldBe` 3
                    Error err -> expectationFailure $ T.unpack err

            it "Creating time entry for non-existent project returns appropriate error" $ \c -> do
                newTimeEntry c "nonexistent"
                selectActiveTracking c `shouldReturn` Success Nothing

            it "Creating time entry with project having no externalId works correctly" $ \c -> do
                insertProject c exampleProjectNoExt
                newTimeEntry c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProjectNoExt)

            it "stopTimeEntry stops running entry" $ \c -> do
                insertProject c exampleProject
                newTimeEntry c "name"
                stopTimeEntry c True
                selectTimeEntries c "name" >>= \case
                    Success res -> isJust (endtime (head res)) `shouldBe` True
                    Error err -> expectationFailure $ T.unpack err

            it "Returns error when trying to create entry for project with invalid name" $ \c -> do
                (maybeErr, output) <- newTimeEntry c "nonexistent"
                T.isInfixOf "Error" output `shouldBe` True
                output `shouldNotBe` "Using provided project name. Started tracking time for project: nonexistent"

            it "Stopping time entry when none is running returns appropriate message" $ \c -> do
                result <- stopTimeEntry c False
                result `shouldBe` Just "Nothing to stop. No time is being tracked"

            it "Consecutive stop operations should handle gracefully" $ \c -> do
                insertProject c exampleProject
                newTimeEntry c "name"
                stopTimeEntry c False
                result <- stopTimeEntry c False
                result `shouldBe` Just "Nothing to stop. No time is being tracked"

            it "newTimeEntry returns appropriate messages" $ \c -> do
                insertProject c exampleProject
                (selectOut, updateOut) <- newTimeEntry c "name"
                selectOut `shouldBe` Nothing
                updateOut `shouldBe` "Using provided project name. Started tracking time for project: name"

                (selectOut2, updateOut2) <- newTimeEntry c "name"
                selectOut2 `shouldNotBe` Nothing
                T.isInfixOf "Finalized time entry for name" (fromJust selectOut2) `shouldBe` True

            it "Project names should be case sensitive" $ \c -> do
                insertProject c exampleProject
                insertProject c Project{name = "NAME", externalId = Just "id2"}

                newTimeEntry c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProject)

                newTimeEntry c "NAME"
                selectActiveTracking c `shouldReturn` Success (Just Project{name = "NAME", externalId = Just "id2"})

            it "Empty project name should be handled gracefully" $ \c -> do
                (_, output) <- newTimeEntry c ""
                T.isInfixOf "Error" output `shouldBe` True

            it "Starting a new entry for the same project finalizes the old one" $ \c -> do
                insertProject c exampleProject
                newTimeEntry c "name"

                selectTimeEntries c "name" >>= \case
                    Success entries1 -> do
                        length entries1 `shouldBe` 1
                        isJust (endtime (head entries1)) `shouldBe` False
                    Error err -> expectationFailure $ T.unpack err

                newTimeEntry c "name"

                selectTimeEntries c "name" >>= \case
                    Success entries2 -> do
                        length entries2 `shouldBe` 2
                        -- The most recent entry (head) should be active
                        isJust (endtime (head entries2)) `shouldBe` True
                        -- The previous entry should be finalized
                        isJust (endtime (entries2 !! 1)) `shouldBe` False
                    Error err -> expectationFailure $ T.unpack err

            it "After a sequence of operations, only the last started project is active" $ \c -> do
                insertProject c exampleProject
                insertProject c exampleProjectTwo

                newTimeEntry c "name"
                newTimeEntry c "nameTwo"
                selectActiveTracking c `shouldReturn` Success (Just exampleProjectTwo)

                stopTimeEntry c False
                selectActiveTracking c `shouldReturn` Success Nothing

                newTimeEntry c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProject)

            it "stopTimeEntry behaves correctly in standalone vs newTimeEntry context" $ \c -> do
                insertProject c exampleProject

                newTimeEntry c "name"
                directStop <- stopTimeEntry c False
                directStop `shouldBe` Just "Finalized time entry for name."
                selectActiveTracking c `shouldReturn` Success Nothing

                newTimeEntry c "name"
                (indirectStop, _) <- newTimeEntry c "name"
                indirectStop `shouldBe` Just "Finalized time entry for name."
                selectActiveTracking c `shouldReturn` Success (Just exampleProject)

            it "Switching between projects correctly finalizes previous entries" $ \c -> do
                insertProject c exampleProject
                insertProject c exampleProjectTwo

                newTimeEntry c "name"
                selectActiveTracking c `shouldReturn` Success (Just exampleProject)

                newTimeEntry c "nameTwo"
                selectActiveTracking c `shouldReturn` Success (Just exampleProjectTwo)

                selectTimeEntries c "name" >>= \case
                    Success entries -> do
                        length entries `shouldBe` 1
                        isJust (endtime (head entries)) `shouldBe` True
                    Error err -> expectationFailure $ T.unpack err
