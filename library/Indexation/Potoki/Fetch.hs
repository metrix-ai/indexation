module Indexation.Potoki.Fetch where

import Indexation.Prelude
import Indexation.Types
import Potoki.Core.Fetch
import qualified Indexation.IndexTable as A


index :: (Eq value, Hashable value) => IORef (IndexTable value) -> Fetch value -> Fetch (Index value)
index indexTableRef (Fetch valueMaybeIO) =
  Fetch $ do
    valueMaybe <- valueMaybeIO
    case valueMaybe of
      Just value -> do
        indexTable <- readIORef indexTableRef
        A.register value indexTable & \ (!index, !newIndexTable) -> do
          writeIORef indexTableRef newIndexTable
          return (Just index)
      Nothing -> return Nothing
