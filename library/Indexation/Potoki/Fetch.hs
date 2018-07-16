module Indexation.Potoki.Fetch where

import Indexation.Prelude
import Indexation.Types
import Potoki.Core.Fetch
import qualified Indexation.IndexTable as A
import qualified STMContainers.Map as B
import qualified Focus as C


index :: (Eq entity, Hashable entity) => IORef (IndexTable entity) -> Fetch entity -> Fetch (Index entity)
index indexTableRef (Fetch entityMaybeIO) =
  Fetch $ do
    entityMaybe <- entityMaybeIO
    case entityMaybe of
      Just entity -> do
        indexTable <- readIORef indexTableRef
        A.register entity indexTable & \ (!index, !newIndexTable) -> do
          writeIORef indexTableRef newIndexTable
          return (Just index)
      Nothing -> return Nothing

indexConcurrently :: (Eq entity, Hashable entity) => Indexer entity -> Fetch entity -> Fetch (Index entity)
indexConcurrently (Indexer sizeVar map) (Fetch entityMaybeIO) =
  Fetch $ do
    entityMaybe <- entityMaybeIO
    case entityMaybe of
      Just entity -> fmap Just $ atomically $ B.focus strategy entity map
      Nothing -> return Nothing
  where
    strategy = \ case
      Just indexInt -> return (Index indexInt, C.Keep)
      Nothing -> do
        size <- readTVar sizeVar
        writeTVar sizeVar $! succ size
        return (Index size, C.Replace size)
