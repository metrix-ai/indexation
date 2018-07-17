module Indexation.Potoki.Transform where

import Indexation.Prelude hiding (runState)
import Indexation.Types
import Potoki.Core.Transform
import qualified Indexation.Potoki.Fetch as A


indexConcurrently :: (Eq entity, Hashable entity) => Indexer entity -> Transform entity (Index entity)
indexConcurrently indexer = Transform (return . A.indexConcurrently indexer)
