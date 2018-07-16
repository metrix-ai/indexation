module Indexation.EntityTable
(
  EntityTable,
  indexTable,
  lookup,
)
where

import Indexation.Prelude hiding (lookup)
import Indexation.Types
import qualified Indexation.Vector as A
import qualified Data.Vector as B
import qualified Indexation.Cereal.Put as C
import qualified Indexation.Cereal.Get as D
import qualified Data.Serialize as E


instance E.Serialize entity => E.Serialize (EntityTable entity) where
  put = C.putEntityTable E.put
  get = D.getEntityTable E.get

indexTable :: IndexTable entity -> EntityTable entity
indexTable (IndexTable size table) =
  EntityTable vector
  where
    vector =
      A.indexHashMapWithSize size table

lookup :: Index entity -> EntityTable entity -> Maybe entity
lookup (Index indexPrim) (EntityTable vector) =
  vector B.!? indexPrim
