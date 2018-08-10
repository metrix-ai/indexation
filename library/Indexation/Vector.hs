module Indexation.Vector
where

import Indexation.Prelude
import Data.Vector
import qualified Data.Vector.Mutable as A
import qualified Data.HashMap.Strict as B
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified ListT


{-|
This function is not tested.
-}
{-# NOINLINE populate #-}
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

{-# NOINLINE unfoldM #-}
unfoldM :: Monad m => Int -> UnfoldM m (Int, element) -> m (Vector element)
unfoldM size unfoldM =
  let
    step mv (index, element) = return (unsafeDupablePerformIO (A.write mv index element $> mv))
    in do
      !mv <- return (unsafeDupablePerformIO (A.unsafeNew size))
      UnfoldM.foldlM' step mv unfoldM
      !iv <- return (unsafeDupablePerformIO (unsafeFreeze mv))
      return iv

listTInIO :: Int -> ListT IO (Int, element) -> IO (Vector element)
listTInIO size listT = do
  mv <- A.unsafeNew size
  flip ListT.traverse_ listT $ \ (index, element) -> A.write mv index element
  unsafeFreeze mv
