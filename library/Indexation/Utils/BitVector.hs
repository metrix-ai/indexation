module Indexation.Utils.BitVector
where

import Indexation.Prelude hiding (Vector)
import Data.Vector.Unboxed hiding (forM_)
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector
import qualified Data.Bit as Bit


filledIndicesFoldable :: Foldable foldable => Int -> foldable Int -> Vector Bit
filledIndicesFoldable size foldable = runST $ do
  mv <- MutableUnboxedVector.new size
  forM_ foldable $ \ index -> do
    MutableUnboxedVector.unsafeWrite mv index (Bit.fromBool True)
  unsafeFreeze mv
