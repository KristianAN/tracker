module Project (Project (..)) where

import Data.Text qualified as T
import Data.Time.LocalTime

-- | The name is the unique identifier for a project
data Project = Project
    { name :: T.Text
    , externalId :: Maybe T.Text
    }
    deriving (Show, Eq)

{- | A time entry has a start and stop LocalTime and a project name
| Sqlite has RowID which we can use to reference a unique entry
-}
data TimeEntry = TimeEntry
    { projectName :: T.Text
    , startTime :: LocalTime
    , endtime :: Maybe LocalTime
    }
