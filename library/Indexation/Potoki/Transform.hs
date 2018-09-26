module Indexation.Potoki.Transform
(
  Transform,
  Index(..),
  Indexer,
  EntityTable,
  ReindexTable,
  index,
  reindex,
  lookup,
)
where

import Indexation.Prelude hiding (runState, index, lookup)
import Indexation.Types
import Indexation.Instances ()
import Potoki.Transform
import qualified Focus
import qualified StmContainers.Map as StmMap
import qualified Data.Vector as Vector
import qualified Indexation.Functions as Functions


index :: (Eq entity, Hashable entity, Num prim) => Indexer entity -> Transform entity (Index prim entity)
index (Indexer sizeVar map) =
  {-# SCC "index" #-} 
  mapInIO $ \ entity -> atomically $ StmMap.focus focus entity map
  where
    focus = Focus.Focus conceal reveal where
      conceal = do
        size <- readTVar sizeVar
        writeTVar sizeVar $! succ size
        return (Index (fromIntegral size), Focus.Set size)
      reveal indexInt = return (Index (fromIntegral indexInt), Focus.Leave)

reindex :: Integral prim => ReindexTable prim entity -> Transform (Index prim entity) (Index prim entity)
reindex table = mapFilter (\ oldIndex -> Functions.lookupNewIndex oldIndex table)

lookup :: Integral prim => EntityTable entity -> Transform (Index prim entity) (Maybe entity)
lookup (EntityTable entityTableVector) =
  {-# SCC "lookup" #-} 
  arr $ \ (Index indexPrim) -> let
    indexInt = fromIntegral indexPrim
    in if Vector.length entityTableVector > indexInt
      then Just $! Vector.unsafeIndex entityTableVector indexInt
      else Nothing
