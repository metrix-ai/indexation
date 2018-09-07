module Indexation.FoldM.Basic
where

import Indexation.Prelude
import Control.Foldl
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector


countIndices :: Int -> FoldM IO Int (UnboxedVector.Vector Word32)
countIndices amount = FoldM step init extract where
  init = MutableUnboxedVector.new amount
  step mv index = MutableUnboxedVector.modify mv succ index $> mv
  extract = UnboxedVector.unsafeFreeze
