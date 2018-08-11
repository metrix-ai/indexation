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
  readIndexTableFromFile,
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
import qualified Data.HashTable.IO as HashtablesIO
import qualified Data.Serialize as Cereal
import qualified Data.ByteString as ByteString


createIndexer :: IO (Indexer entity)
createIndexer = do
  sizeRef <- newIORef 0
  map <- HashtablesIO.new
  return (Indexer sizeRef map)

getIndexerSize :: Indexer entity -> IO Int
getIndexerSize (Indexer sizeRef _) = readIORef sizeRef

freezeIndexerAsVector :: Indexer entity -> IO (Vector entity)
freezeIndexerAsVector (Indexer sizeRef map) =
  do
    size <- readIORef sizeRef
    Vector.hashTable size map

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
    bytes <- withFile filePath ReadMode (flip ByteString.hGet 8)
    Cereal.runGet Cereal.getInt64le bytes & \ case
      Right x -> return (fromIntegral x)
      Left x -> error ("Unexpected binary parsing error: " <> x)

readEntityTableFromFile :: Serialize entity => FilePath -> IO (Either IOException (Either Text (EntityTable entity)))
readEntityTableFromFile filePath =
  PotokiIo.produceAndConsume
    (PotokiProduce.fileBytes filePath)
    (right' (PotokiConsume.get Cereal.get))

readIndexTableFromFile :: (Serialize entity, Eq entity, Hashable entity) => FilePath -> IO (Either IOException (Either Text (IndexTable entity)))
readIndexTableFromFile filePath =
  PotokiIo.produceAndConsume
    (PotokiProduce.fileBytes filePath)
    (right' (PotokiConsume.get Cereal.get))
