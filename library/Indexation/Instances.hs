module Indexation.Instances
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()
import Indexation.Functions
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Bit (Bit)
import qualified Data.Vector.Unboxed as UVec


instance Show prim => Show (Index prim a) where show (Index x) = show x
deriving instance Eq prim => Eq (Index prim a)
deriving instance Ord prim => Ord (Index prim a)
deriving instance Hashable prim => Hashable (Index prim a)
deriving instance Generic (Index prim a)
instance NFData prim => NFData (Index prim a)
derivingUnbox "Index"
  [t| forall prim a. UVec.Unbox prim => Index prim a -> prim |]
  [| \ (Index x) -> x |]
  [| Index |]
derivingUnbox "MaybeIndex"
  [t| forall prim a. (Num prim, UVec.Unbox prim) => Maybe (Index prim a) -> (Bit, Index prim a) |]
  [| maybe (0, Index 0) (\ x -> (1, x)) |]
  [| \ case (1, x) -> Just x; _ -> Nothing |]


instance Show (IndexSet a) where
  show = show . (\ (IndexSet x) -> x)
