module Indexation.Instances.Cereal
where

import Indexation.Prelude
import Indexation.Types
import Data.Serialize
import qualified Indexation.Cereal.Get as Get
import qualified Indexation.Cereal.Put as Put


instance Serialize (Index a) where
  get = Get.getIndex
  put = Put.putIndex
