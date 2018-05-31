module Indexation.HashMap
where

import Indexation.Prelude
import Data.HashMap.Strict


traverse_ :: Applicative effect => (k -> v -> effect ()) -> HashMap k v -> effect ()
traverse_ effect =
  foldrWithKey step init
  where
    init = pure ()
    step k v acc = effect k v *> acc
