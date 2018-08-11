module Indexation.Potoki.Transform
(
  Transform,
  Index(..),
  Indexer,
  EntityTable,
  index,
  lookup,
)
where

import Indexation.Prelude hiding (runState, index, lookup)
import Indexation.Types
import Indexation.Instances ()
import Potoki.Transform
import qualified Focus
import qualified Data.HashTable.IO as HashtablesIO
import qualified Data.Vector as Vector


index :: (Eq entity, Hashable entity) => Indexer entity -> Transform entity (Index entity)
index (Indexer sizeRef map) =
  mapInIO $ \ entity -> HashtablesIO.mutateIO map entity mutation
  where
    mutation = \ case
      Just indexInt -> return (Just indexInt, Index indexInt) 
      Nothing -> do
        size <- readIORef sizeRef
        writeIORef sizeRef $! succ size
        return (Just size, Index size)

lookup :: EntityTable entity -> Transform (Index entity) (Maybe entity)
lookup (EntityTable entityTableVector) =
  arr $ \ (Index indexInt) -> if Vector.length entityTableVector > indexInt
    then Just $! Vector.unsafeIndex entityTableVector indexInt
    else Nothing
