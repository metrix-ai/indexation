module Indexation.IO
(
  indexProduceToFiles,
  readEntitiesAmountFromEntityTableFile,
)
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances.Cereal ()
import qualified Potoki.Core.IO as PotokiIo
import qualified Potoki.Core.Consume as PotokiConsume
import qualified Potoki.Cereal.Consume as PotokiConsume
import qualified Potoki.Cereal.Produce as PotokiProduce
import qualified Indexation.Potoki.Transform as PotokiTransform
import qualified Indexation.Vector as Vector
import qualified STMContainers.Map as StmMap
import qualified Data.Serialize as Cereal
import qualified Data.ByteString as ByteString


createIndexer :: IO (Indexer entity)
createIndexer = do
  sizeVar <- newTVarIO 0
  map <- StmMap.newIO
  return (Indexer sizeVar map)

createIndexerVector :: Indexer entity -> IO (Vector entity)
createIndexerVector (Indexer sizeVar map) =
  atomically $ do
    size <- readTVar sizeVar
    Vector.listT size (fmap swap (StmMap.stream map))

createIndexerEntityTable :: Indexer entity -> IO (EntityTable entity)
createIndexerEntityTable = fmap EntityTable . createIndexerVector

serializeProduceEntityIndicesToFile :: (Eq entity, Hashable entity) => FilePath -> Indexer entity -> Produce entity -> IO (Either IOException ())
serializeProduceEntityIndicesToFile path indexer entityProduce =
  PotokiIo.produceAndConsume entityProduce $
  PotokiConsume.transform (PotokiTransform.indexConcurrently indexer) $
  PotokiConsume.encodeToFile path

serializeIndexerToFile :: Serialize entity => FilePath -> Indexer entity -> IO (Either IOException ())
serializeIndexerToFile path indexer = do
  entityTable <- createIndexerEntityTable indexer
  let
    produce = PotokiProduce.put (Cereal.put entityTable)
    consume = PotokiConsume.writeBytesToFile path
    in PotokiIo.produceAndConsume produce consume

indexProduceToFiles :: (Eq entity, Hashable entity, Serialize entity) => FilePath {-^ Entity table -} -> FilePath {-^ Index stream -} -> Produce entity -> IO (Either IOException ())
indexProduceToFiles entityTablePath indexStreamPath entityProduce =
  do
    indexer <- createIndexer
    runExceptT $ do
      ExceptT (serializeProduceEntityIndicesToFile indexStreamPath indexer entityProduce)
      ExceptT (serializeIndexerToFile entityTablePath indexer)

readEntitiesAmountFromEntityTableFile :: FilePath -> IO (Either IOException Int)
readEntitiesAmountFromEntityTableFile filePath =
  try $ do
    bytes <- withFile filePath ReadMode (flip ByteString.hGet 4)
    Cereal.runGet Cereal.getInt64le bytes & \ case
      Right x -> return (fromIntegral x)
      Left x -> error ("Unexpected binary parsing error: " <> x)
