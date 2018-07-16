module Indexation.IndexTable
(
  IndexTable,
  lookup,
  register,
  empty,
)
where

import Indexation.Prelude hiding (lookup, empty)
import Indexation.Types
import qualified Data.HashMap.Strict as A
import qualified Indexation.Cereal.Get as B
import qualified Indexation.Cereal.Put as C
import qualified Indexation.EntityTable as D
import qualified Data.Serialize as E


instance (E.Serialize entity, Eq entity, Hashable entity) => E.Serialize (IndexTable entity) where
  put = C.putEntityTable E.put . D.indexTable
  get = B.getIndexTableAsEntityTable E.get

lookup :: (Eq entity, Hashable entity) => entity -> IndexTable entity -> Maybe (Index entity)
lookup entity (IndexTable size hashMap) =
  fmap Index (A.lookup entity hashMap)

register :: (Eq entity, Hashable entity) => entity -> IndexTable entity -> (Index entity, IndexTable entity)
register entity (IndexTable size hashMap) =
  A.lookup entity hashMap & \ case
    Just index -> (Index index, IndexTable size hashMap)
    Nothing -> let
      newSize = succ size
      newHashMap = A.insert entity size hashMap
      index = Index size
      newTable = IndexTable newSize newHashMap
      in (index, newTable)

empty :: (Eq entity, Hashable entity) => IndexTable entity
empty =
  IndexTable 0 mempty
