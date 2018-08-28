module Indexation.UnboxedVector
where

import Indexation.Prelude hiding (Vector)
import Data.Vector.Unboxed hiding (forM_)
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector


filledIndicesFoldable :: Foldable foldable => Int -> foldable Int -> Vector Bool
filledIndicesFoldable size foldable = runST $ do
  mv <- MutableUnboxedVector.new size
  forM_ foldable $ \ index -> do
    MutableUnboxedVector.unsafeWrite mv index True
  unsafeFreeze mv
