module Indexation.Instances
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()
import Indexation.Functions
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Bit (Bit)


instance Show (Index a) where show (Index int) = show int
deriving instance Eq (Index a)
deriving instance Ord (Index a)
deriving instance Hashable (Index a)
deriving instance Generic (Index a)
instance NFData (Index a)
derivingUnbox "Index"
  [t| forall a. Index a -> Int |]
  [| \ (Index x) -> x |]
  [| Index |]
derivingUnbox "MaybeIndex"
  [t| forall a. Maybe (Index a) -> (Bit, Index a) |]
  [| maybe (0, Index 0) (\ x -> (1, x)) |]
  [| \ case (1, x) -> Just x; _ -> Nothing |]


instance Show (IndexSet a) where
  show = show . (\ (IndexSet x) -> x)
