module Indexation.Potoki.Produce where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()
import Potoki.Produce
import Potoki.Cereal.Produce


indicesFromFile :: FilePath -> Produce (Either IOException (Either Text (Index a)))
indicesFromFile path =
  fileDecoded path
