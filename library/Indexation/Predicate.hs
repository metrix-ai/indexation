module Indexation.Predicate
where

import Indexation.Prelude hiding (not)
import Indexation.Types
import Indexation.Instances ()
import qualified Indexation.Prelude as Prelude
import qualified Indexation.Utils.UnboxedVector as UnboxedVector
import qualified Indexation.Functions as Functions


negated :: Predicate a -> Predicate a
negated (Predicate fn) = Predicate (Prelude.not . fn)

inIndexSet :: IndexSet a -> Predicate (Index a)
inIndexSet indexSet = Predicate (\ index -> Functions.lookupInIndexSet index indexSet)
