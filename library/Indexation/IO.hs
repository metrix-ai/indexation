module Indexation.IO
(
  Indexer,
  EntityTable,
  createIndexer,
  getIndexerSize,
  freezeIndexerAsEntityTable,
  serializeEntityTableToFile,
  serializeIndexerToFile,
  readEntitiesAmountFromEntityTableFile,
  readEntityTableFromFile,
)
where

import Indexation.Prelude
import Indexation.Types
import Indexation.Instances ()
import qualified Potoki.IO as PotokiIo
import qualified Potoki.Produce as PotokiProduce
import qualified Potoki.Consume as PotokiConsume
import qualified Potoki.Cereal.Consume as PotokiConsume
import qualified Potoki.Cereal.Produce as PotokiProduce
import qualified Indexation.Potoki.Transform as PotokiTransform
import qualified Indexation.Vector as Vector
import qualified StmContainers.Map as StmMap
import qualified Data.Serialize as Cereal
import qualified Data.ByteString as ByteString


createIndexer :: IO (Indexer entity)
createIndexer = do
  sizeVar <- newTVarIO 0
  map <- StmMap.newIO
  return (Indexer sizeVar map)

getIndexerSize :: Indexer entity -> IO Int
getIndexerSize (Indexer sizeVar _) = atomically (readTVar sizeVar)

freezeIndexerAsVector :: Indexer entity -> IO (Vector entity)
freezeIndexerAsVector (Indexer sizeVar map) =
  atomically $ do
    size <- readTVar sizeVar
    Vector.unfoldM size (fmap swap (StmMap.unfoldM map))

freezeIndexerAsEntityTable :: Indexer entity -> IO (EntityTable entity)
freezeIndexerAsEntityTable = fmap EntityTable . freezeIndexerAsVector

serializeEntityTableToFile :: Serialize entity => EntityTable entity -> FilePath -> IO (Either IOException ())
serializeEntityTableToFile entityTable path = let
  produce = PotokiProduce.put (Cereal.put entityTable)
  consume = PotokiConsume.writeBytesToFile path
  in PotokiIo.produceAndConsume produce consume

serializeIndexerToFile :: Serialize a => (Text -> IO ()) -> FilePath -> Indexer a -> IO (Either IOException ())
serializeIndexerToFile log file indexer = do
  log "Freezing"
  entityTable <- freezeIndexerAsEntityTable indexer
  log "Writing to file"
  serializeEntityTableToFile entityTable file

readEntitiesAmountFromEntityTableFile :: FilePath -> IO (Either IOException Int)
readEntitiesAmountFromEntityTableFile filePath =
  try $ do
    bytes <- withFile filePath ReadMode (flip ByteString.hGet 4)
    Cereal.runGet Cereal.getInt64le bytes & \ case
      Right x -> return (fromIntegral x)
      Left x -> error ("Unexpected binary parsing error: " <> x)

readEntityTableFromFile :: Serialize entity => FilePath -> IO (Either IOException (Either Text (EntityTable entity)))
readEntityTableFromFile filePath =
  PotokiIo.produceAndConsume
    (PotokiProduce.fileBytes filePath)
    (right' (PotokiConsume.get Cereal.get))
