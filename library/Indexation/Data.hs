module Indexation.Data
(
  Index(..),
  EntityTable,
  Indexer,
  lookupEntity,
)
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances ()
import qualified Data.Vector as Vector


lookupEntity :: Index entity -> EntityTable entity -> Maybe entity
lookupEntity (Index indexPrim) (EntityTable vector) =
  vector Vector.!? indexPrim
