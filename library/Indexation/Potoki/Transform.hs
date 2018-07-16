module Indexation.Potoki.Transform where

import Indexation.Prelude hiding (runState)
import Indexation.Types
import Potoki.Core.Transform
import qualified Indexation.Potoki.Fetch as A
import qualified Indexation.IndexTable as B
import qualified STMContainers.Map as C


indexConcurrently :: (Eq entity, Hashable entity) => Indexer entity -> Transform entity (Index entity)
indexConcurrently indexer = Transform (return . A.indexConcurrently indexer)

indexWithRef :: (Eq entity, Hashable entity) => IORef (IndexTable entity) -> Transform entity (Index entity)
indexWithRef indexTableRef =
  Transform $ \ fetch -> return (A.index indexTableRef fetch)

{-|
Index the entities emitting index and updated index table on each step.
-}
indexWithState :: (Eq entity, Hashable entity) => Transform entity (Index entity, IndexTable entity)
indexWithState =
  runState (\ entity -> state (B.register entity)) B.empty
