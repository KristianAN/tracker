module Models.TimeEntry (TimeEntry (..)) where

import Data.Text qualified as T
import Data.Time.LocalTime

{- | A time entry has a start and stop LocalTime and a project name
| Sqlite has RowID which we can use to reference a unique entry
-}
data TimeEntry = TimeEntry
    { projectName :: T.Text
    , startTime :: LocalTime
    , endtime :: Maybe LocalTime
    , rowId :: Maybe Int
    }
    deriving (Show, Eq)
