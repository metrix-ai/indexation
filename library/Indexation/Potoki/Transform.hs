module Indexation.Potoki.Transform where

import Indexation.Prelude hiding (runState)
import Indexation.Types
import Potoki.Core.Transform
import qualified Indexation.Potoki.Fetch as A
import qualified Indexation.IndexTable as B


indexWithRef :: (Eq value, Hashable value) => IORef (IndexTable value) -> Transform value (Index value)
indexWithRef indexTableRef =
  Transform $ \ fetch -> return (A.index indexTableRef fetch)

{-|
Index the entities emitting index and updated index table on each step.
-}
indexWithState :: (Eq value, Hashable value) => Transform value (Index value, IndexTable value)
indexWithState =
  runState (\ value -> state (B.register value)) B.empty
