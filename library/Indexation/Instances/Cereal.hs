module Indexation.Instances.Cereal
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize
import qualified Indexation.Cereal.Get as Get
import qualified Indexation.Cereal.Put as Put
import qualified Indexation.Functions as Functions


instance Serialize prim => Serialize (Index prim a) where
  get = Index <$> get
  put = put . Functions.indexPrim

instance Serialize entity => Serialize (EntityTable entity) where
  get = Get.getEntityTable get
  put = Put.putEntityTable put

instance (Serialize entity, Eq entity, Hashable entity, Integral prim) => Serialize (IndexTable prim entity) where
  get = Get.getIndexTableAsEntityTable get
  put = Put.putEntityTable put . Functions.indexTableEntityTable

deriving instance Serialize (IndexSet a)

deriving instance Serialize (IndexCounts a)
