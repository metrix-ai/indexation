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
import Indexation.Instances.Cereal ()
import qualified Data.HashMap.Strict as A


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
