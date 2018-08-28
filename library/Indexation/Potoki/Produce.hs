module Indexation.Potoki.Produce
(
  entities,
) where

import Indexation.Prelude
import Indexation.Types
import qualified Potoki.Produce as Produce

entities :: EntityTable a -> Produce a
entities (EntityTable vector) = Produce.vector vector
