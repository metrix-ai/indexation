module Indexation.Instances
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()
import Indexation.Functions
import qualified Data.Vector.Unboxed as UnboxedVector


instance Show (Index a) where show (Index int) = show int
deriving instance Eq (Index a)
deriving instance Ord (Index a)
deriving instance Hashable (Index a)

instance Show (IndexSet a) where
  show = show . UnboxedVector.ifoldr (\ i -> bool id (i :)) [] . (\ (IndexSet x) -> x)
