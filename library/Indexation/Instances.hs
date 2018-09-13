module Indexation.Instances
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()
import Indexation.Functions


instance Show (Index a) where show (Index int) = show int
deriving instance Eq (Index a)
deriving instance Ord (Index a)
deriving instance Hashable (Index a)
deriving instance Generic (Index a)
instance NFData (Index a)

instance Show (IndexSet a) where
  show = show . (\ (IndexSet x) -> x)
