module Indexation.Instances
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()


deriving instance Show (Index a)
deriving instance Eq (Index a)
deriving instance Ord (Index a)
deriving instance Hashable (Index a)
