module Network.MDNS.Decoder where

import qualified Data.Binary.Get           as SG
import qualified Data.Binary.Strict.BitGet as BG

import           Control.Monad
import           Data.Int
import           Network.MDNS.Internal

parseHeader :: SG.Get Header
parseHeader = do
  headerId' <- SG.getWord16be
  flags' <- SG.getByteString 2
  qdCount' <- SG.getInt16be
  anCount' <- SG.getInt16be
  nsCount' <- SG.getInt16be
  arCount' <- SG.getInt16be
  questions <- getQuestion (fromIntegral (qdCount' :: Int16) :: Int)
  resources <- getResourceRecords (fromIntegral (anCount' :: Int16) :: Int)

  let flag' = BG.runBitGet flags' parseFlag

  pure $ Header headerId' flag' qdCount' anCount' nsCount' arCount' questions resources

-- TODO remove BitGet and make it a normal Get
parseFlag :: BG.BitGet Flag
parseFlag = Flag
  <$> BG.getBit
  <*> BG.getAsWord8 4
  <*> BG.getBit
  <*> BG.getBit
  <*> BG.getBit
  <*> BG.getBit
  <*> (BG.getAsWord8 3 >> BG.getAsWord8 4) -- field z is for future use, so we skip it

getQuestion :: Int -> SG.Get [Question]
getQuestion count' = replicateM count' parseQuestion

parseQuestion :: SG.Get Question
parseQuestion = do
  lenghtName <- fromIntegral <$> SG.getWord8
  qname <- SG.getByteString lenghtName

  lenghtType <- fromIntegral <$> SG.getWord8
  qtype <- SG.getByteString lenghtType

  -- TODO Unicast Response
  -- https://en.wikipedia.org/wiki/Multicast_DNS#Queries
  lengthClass <- fromIntegral <$> SG.getWord8
  qclass <- SG.getByteString lengthClass
  pure $ Question qname qtype qclass

getResourceRecords :: Int -> SG.Get [Resource]
getResourceRecords count' = replicateM count' parseResource

parseResource :: SG.Get Resource
parseResource = do
  lenghtName <- fromIntegral <$> SG.getWord8
  rName <- SG.getByteString lenghtName

  lenghtType <- fromIntegral <$> SG.getWord8
  rType <- SG.getByteString lenghtType

  -- TODO Cache-Flush
  -- https://en.wikipedia.org/wiki/Multicast_DNS#Resource_Records
  lenghtClass <- fromIntegral <$> SG.getWord8
  rClass <- SG.getByteString lenghtClass

  rTTL <- SG.getInt32be
  rRDLength <- SG.getInt16be

  lengthData <- fromIntegral <$> SG.getWord8
  rRData <- SG.getByteString lengthData

  pure $ Resource rName rType rClass rTTL rRDLength rRData
