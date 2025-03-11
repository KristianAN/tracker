{-# LANGUAGE OverloadedStrings #-}

module Models.Project (Project (..),  prettyPrintProject) where

import Data.Maybe
import Data.Text qualified as T

-- | The name is the unique identifier for a project
data Project = Project
    { name :: T.Text
    , externalId :: Maybe T.Text
    }
    deriving (Show, Eq)

prettyPrintProject :: Project -> T.Text
prettyPrintProject project =
    "Name: " <> name project <> ". External id: " <> extIdText <> "."
  where
    extIdText = fromMaybe "undefined external id" (externalId project)

