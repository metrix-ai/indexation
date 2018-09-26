module Indexation.Predicate
where

import Indexation.Prelude hiding (not)
import Indexation.Types
import Indexation.Instances ()
import qualified Indexation.Prelude as Prelude
import qualified Indexation.Functions as Functions


negated :: Predicate a -> Predicate a
negated (Predicate fn) = Predicate (Prelude.not . fn)

inIndexSet :: Integral prim => IndexSet a -> Predicate (Index prim a)
inIndexSet indexSet = Predicate (\ index -> Functions.lookupInIndexSet index indexSet)
