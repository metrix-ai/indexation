module Indexation.Data
(
  Index(..),
  EntityTable,
  IndexTable,
  Indexer,
  lookupEntity,
  lookupIndex,
)
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances ()
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap


lookupEntity :: Index entity -> EntityTable entity -> Maybe entity
lookupEntity (Index indexPrim) (EntityTable vector) =
  vector Vector.!? indexPrim

lookupIndex :: (Eq entity, Hashable entity) => entity -> IndexTable entity -> Maybe (Index entity)
lookupIndex entity (IndexTable _ hashMap) =
  fmap Index (HashMap.lookup entity hashMap)
