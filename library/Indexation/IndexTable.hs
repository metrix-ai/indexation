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
import qualified Indexation.ValueTable as D
import qualified Data.Serialize as E


instance (E.Serialize value, Eq value, Hashable value) => E.Serialize (IndexTable value) where
  put = C.putValueTable E.put . D.indexTable
  get = B.getIndexTableAsValueTable E.get

lookup :: (Eq value, Hashable value) => value -> IndexTable value -> Maybe (Index value)
lookup value (IndexTable size hashMap) =
  fmap Index (A.lookup value hashMap)

register :: (Eq value, Hashable value) => value -> IndexTable value -> (Index value, IndexTable value)
register value (IndexTable size hashMap) =
  A.lookup value hashMap & \ case
    Just index -> (Index index, IndexTable size hashMap)
    Nothing -> let
      newSize = succ size
      newHashMap = A.insert value size hashMap
      index = Index size
      newTable = IndexTable newSize newHashMap
      in (index, newTable)

empty :: (Eq value, Hashable value) => IndexTable value
empty =
  IndexTable 0 mempty
