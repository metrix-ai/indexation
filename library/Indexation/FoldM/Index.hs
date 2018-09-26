module Indexation.FoldM.Index
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Data
import Control.Foldl
import qualified Indexation.FoldM.Basic as Basic


indexCounts :: Integral prim => Int -> FoldM IO (Index prim a) (IndexCounts a)
indexCounts amount = dimap (fromIntegral . indexPrim) IndexCounts (Basic.countIndices amount)
