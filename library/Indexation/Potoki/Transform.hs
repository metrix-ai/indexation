module Indexation.Potoki.Transform where

import Indexation.Prelude hiding (runState)
import Indexation.Types
import Potoki.Transform
import qualified Focus
import qualified StmContainers.Map as StmMap


indexConcurrently :: (Eq entity, Hashable entity) => Indexer entity -> Transform entity (Index entity)
indexConcurrently (Indexer sizeVar map) =
  mapInIO $ \ entity -> atomically $ StmMap.focus focus entity map
  where
    focus = Focus.Focus conceal reveal where
      conceal = do
        size <- readTVar sizeVar
        writeTVar sizeVar $! succ size
        return (Index size, Focus.Set size)
      reveal indexInt = return (Index indexInt, Focus.Leave)
