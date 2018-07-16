module Indexation.Potoki.Fetch where

import Indexation.Prelude
import Indexation.Types
import Potoki.Core.Fetch
import qualified Indexation.IndexTable as A


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
