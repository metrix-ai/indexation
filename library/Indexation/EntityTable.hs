module Indexation.EntityTable
(
  EntityTable,
  indexTable,
  lookup,
)
where

import Indexation.Prelude hiding (lookup)
import Indexation.Types
import Indexation.Instances.Cereal ()
import Indexation.Constructors.EntityTable
import qualified Data.Vector as B


lookup :: Index entity -> EntityTable entity -> Maybe entity
lookup (Index indexPrim) (EntityTable vector) =
  vector B.!? indexPrim
