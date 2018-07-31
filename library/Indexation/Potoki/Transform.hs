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
import qualified StmContainers.Map as StmMap
import qualified Data.Vector as Vector


index :: (Eq entity, Hashable entity) => Indexer entity -> Transform entity (Index entity)
index (Indexer sizeVar map) =
  mapInIO $ \ entity -> atomically $ StmMap.focus focus entity map
  where
    focus = Focus.Focus conceal reveal where
      conceal = do
        size <- readTVar sizeVar
        writeTVar sizeVar $! succ size
        return (Index size, Focus.Set size)
      reveal indexInt = return (Index indexInt, Focus.Leave)

lookup :: EntityTable entity -> Transform (Index entity) (Maybe entity)
lookup (EntityTable entityTableVector) =
  arr $ \ (Index indexInt) -> if Vector.length entityTableVector > indexInt
    then Just $! Vector.unsafeIndex entityTableVector indexInt
    else Nothing
