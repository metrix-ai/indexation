module Indexation.FoldM.Index
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Data
import Control.Foldl
import qualified Indexation.FoldM.Basic as Basic


indexCounts :: Int -> FoldM IO (Index a) (IndexCounts a)
indexCounts amount = dimap (\ (Index x) -> x) IndexCounts (Basic.countIndices amount)
