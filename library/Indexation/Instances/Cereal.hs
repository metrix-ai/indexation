module Indexation.Instances.Cereal
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize
import qualified Indexation.Cereal.Get as Get
import qualified Indexation.Cereal.Put as Put
import qualified Indexation.Constructors.EntityTable as EntityTable


instance Serialize (Index a) where
  get = Get.getIndex
  put = Put.putIndex

instance Serialize entity => Serialize (EntityTable entity) where
  get = Get.getEntityTable get
  put = Put.putEntityTable put

instance (Serialize entity, Eq entity, Hashable entity) => Serialize (IndexTable entity) where
  get = Get.getIndexTableAsEntityTable get
  put = Put.putEntityTable put . EntityTable.indexTable

instance Serialize (IndexSet a) where
  get = IndexSet <$> Get.getVector get
  put (IndexSet vector) = Put.putVector put vector
