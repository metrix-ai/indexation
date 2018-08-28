module Indexation.Vector
where

import Indexation.Prelude
import Data.Vector
import qualified Data.Vector.Mutable as MutableVector
import qualified Data.HashMap.Strict as HashMap
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified ListT


{-|
This function is not tested.
-}
{-# NOINLINE populate #-}
populate :: Monad effect => Int -> effect (Int, element) -> effect (Vector element)
populate size effect =
  do
    mv <- return (unsafeDupablePerformIO (MutableVector.unsafeNew size))
    let
      loop stepsRemaining =
        if stepsRemaining > 0
          then do
            (index, element) <- effect
            () <- return (unsafeDupablePerformIO (MutableVector.write mv index element))
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
  runST $ do
    mv <- MutableVector.new size
    HashMap.foldrWithKey
      (\ element index action -> MutableVector.write mv index element >> action)
      (return ())
      hashMap
    freeze mv

{-# NOINLINE unfoldlM #-}
unfoldlM :: Monad m => Int -> UnfoldlM m (Int, element) -> m (Vector element)
unfoldlM size unfoldlM =
  let
    step mv (index, element) = return (unsafeDupablePerformIO (MutableVector.write mv index element $> mv))
    in do
      !mv <- return (unsafeDupablePerformIO (MutableVector.unsafeNew size))
      UnfoldlM.foldlM' step mv unfoldlM
      !iv <- return (unsafeDupablePerformIO (unsafeFreeze mv))
      return iv

listTInIO :: Int -> ListT IO (Int, element) -> IO (Vector element)
listTInIO size listT = do
  mv <- MutableVector.unsafeNew size
  flip ListT.traverse_ listT $ \ (index, element) -> MutableVector.write mv index element
  unsafeFreeze mv
