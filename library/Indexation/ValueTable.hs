module Indexation.ValueTable
(
  ValueTable,
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


instance E.Serialize value => E.Serialize (ValueTable value) where
  put = C.putValueTable E.put
  get = D.getValueTable E.get

indexTable :: IndexTable value -> ValueTable value
indexTable (IndexTable size table) =
  ValueTable vector
  where
    vector =
      A.indexHashMapWithSize size table

lookup :: Index value -> ValueTable value -> Maybe value
lookup (Index indexPrim) (ValueTable vector) =
  vector B.!? indexPrim
