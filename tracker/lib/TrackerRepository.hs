{-# LANGUAGE OverloadedStrings #-}

module TrackerRepository (insertProject, RepositoryActionResult (..), selectProject) where

import Control.Exception
import Data.Functor
import Data.Text qualified as T
import Database.SQLite.Simple
import Project (Project (..))

data RepositoryActionResult a
    = Success {value :: a}
    | Error {reason :: T.Text}
    deriving (Show, Eq)

insertProject :: Connection -> Project -> IO (RepositoryActionResult ())
insertProject conn project = do
    result <-
        try
            ( execute
                conn
                "insert into project (name, external_id) values (?, ?)"
                ((T.unpack $ name project, externalId project <&> T.unpack) :: (String, Maybe String))
            ) ::
            IO (Either SomeException ())
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right _ -> pure $ Success ()

selectProject :: Connection -> T.Text -> IO (RepositoryActionResult (Maybe Project))
selectProject conn name = do
    result <-
        try
            ( query
                conn
                "select name, external_id from project where name = ?"
                (Only name) ::
                IO [(T.Text, Maybe T.Text)]
            ) ::
            IO (Either SomeException [(T.Text, Maybe T.Text)])
    case result of
        Left exception -> pure $ Error $ T.pack $ show exception
        Right rows -> case rows of
            (pName, extId) : _ ->
                pure $
                    Success $
                        Just Project{name = pName, externalId = extId}
            [] -> pure $ Success Nothing

deleteProject :: Connection -> Project -> IO (RepositoryActionResult ())
deleteProject conn project = undefined
