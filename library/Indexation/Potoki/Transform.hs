module Indexation.Potoki.Transform where

import Indexation.Prelude hiding (runState)
import Indexation.Types
import Potoki.Transform
import qualified Focus
import qualified STMContainers.Map as StmMap


indexConcurrently :: (Eq entity, Hashable entity) => Indexer entity -> Transform entity (Index entity)
indexConcurrently (Indexer sizeVar map) =
  mapInIO $ \ entity -> atomically $ StmMap.focus strategy entity map
  where
    strategy = \ case
      Just indexInt -> return (Index indexInt, Focus.Keep)
      Nothing -> do
        size <- readTVar sizeVar
        writeTVar sizeVar $! succ size
        return (Index size, Focus.Replace size)
