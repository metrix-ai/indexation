module Indexation.Vector
where

import Indexation.Prelude
import Data.Vector
import qualified Data.Vector.Mutable as MutableVector
import qualified Data.HashMap.Strict as HashMap
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified Data.HashTable.IO as HashtablesIO
import qualified Data.HashTable.Class as HashtablesClass


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

{-# NOINLINE unfoldM #-}
unfoldM :: Monad m => Int -> UnfoldM m (Int, element) -> m (Vector element)
unfoldM size unfoldM =
  let
    step mv (index, element) = return (unsafeDupablePerformIO (MutableVector.write mv index element $> mv))
    in do
      !mv <- return (unsafeDupablePerformIO (MutableVector.unsafeNew size))
      UnfoldM.foldlM' step mv unfoldM
      !iv <- return (unsafeDupablePerformIO (unsafeFreeze mv))
      return iv

hashTable :: HashtablesClass.HashTable t => Int -> HashtablesIO.IOHashTable t element Int -> IO (Vector element)
hashTable size hashTable = do
  mv <- MutableVector.unsafeNew size
  flip HashtablesIO.mapM_ hashTable $ \ (element, index) -> MutableVector.write mv index element
  unsafeFreeze mv
