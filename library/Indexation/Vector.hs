module Indexation.Vector
where

import Indexation.Prelude
import Data.Vector
import qualified Data.Vector.Mutable as A
import qualified Data.HashMap.Strict as B


{-|
This function is not tested.
-}
{-# INLINE populate #-}
populate :: Monad effect => Int -> effect (Int, element) -> effect (Vector element)
populate size effect =
  do
    mv <- return (unsafeDupablePerformIO (A.unsafeNew size))
    let
      loop stepsRemaining =
        if stepsRemaining > 0
          then do
            (index, element) <- effect
            () <- return (unsafeDupablePerformIO (A.write mv index element))
            loop (pred stepsRemaining)
          else do
            !v <- return (unsafeDupablePerformIO (freeze mv))
            return v
      in loop size

{-|
This function is partial. It doesn't check the size or indices.
-}
{-# INLINE indexHashMapWithSize #-}
indexHashMapWithSize :: Int -> HashMap element Int -> Vector element
indexHashMapWithSize size hashMap =
  unsafePerformIO $ do
    mv <- A.unsafeNew size
    let
      step () element index = unsafeDupablePerformIO (A.write mv index element)
      !() = B.foldlWithKey' step () hashMap
      in freeze mv
