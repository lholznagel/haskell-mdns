module HeaderParser ( parseHeader ) where

import qualified Data.Binary.Get           as G
import qualified Data.Binary.Strict.BitGet as BG

import           Data.ByteString
import           Data.Int
import           Data.Word

data Flag = Flag
  {
    qr     :: !Bool
  , opcode :: !Word8
  , aa     :: !Bool
  , tc     :: !Bool
  , rd     :: !Bool
  , ra     :: !Bool
  , rcode  :: !Word8
  } deriving (Show)

data Question = Question
  {
    questionName  :: ByteString
  , questionType  :: ByteString
  , questionClass :: ByteString
  } deriving (Show)

data Resource = Resource
  {
    resourceName     :: ByteString
  , resourceType     :: Word8
  , resourceClass    :: Word8
  , resourceTTL      :: Int16
  , resourceRDLenght :: Int16
  --, resourceRData    :: ByteString
  } deriving (Show)

data Header = Header
  {
    id       :: !Word16
  , flag     :: !(Either String Flag)
  , qdCount  :: !Int16
  , anCount  :: !Int16
  , nsCount  :: !Int16
  , arCount  :: !Int16
  , question :: !Question
  , resource :: !Resource
  } deriving (Show)

parseHeader :: G.Get Header
parseHeader = do
  id <- G.getWord16be
  flags <- G.getByteString 2
  qdCount <- G.getInt16be
  anCount <- G.getInt16be
  nsCount <- G.getInt16be
  arCount <- G.getInt16be
  questions <- parseQuestion
  resources <- parseResource

  let flag = BG.runBitGet flags parseFlag

  pure $ Header id flag qdCount anCount nsCount arCount questions resources

-- TODO remove BitGet and make it a normal Get
parseFlag :: BG.BitGet Flag
parseFlag = do
  qr <- BG.getBit
  opcode <- BG.getAsWord8 4
  aa <- BG.getBit
  tc <- BG.getBit
  rd <- BG.getBit
  ra <- BG.getBit

  -- field z is for future use, so we need to skip it
  _ <- BG.getAsWord8 3
  rcode <- BG.getAsWord8 4

  pure $ Flag qr opcode aa tc rd ra rcode

parseQuestion :: G.Get Question
parseQuestion = do
  lenghtName <- fromIntegral <$> G.getWord8
  qname <- G.getByteString lenghtName

  lenghtType <- fromIntegral <$> G.getWord8
  qtype <- G.getByteString lenghtType

  -- TODO Unicast Response
  -- https://en.wikipedia.org/wiki/Multicast_DNS#Queries
  lengthClass <- fromIntegral <$> G.getWord8
  qclass <- G.getByteString lengthClass
  pure $ Question qname qtype qclass

parseResource :: G.Get Resource
parseResource = do
  lenghtName <- fromIntegral <$> G.getWord8
  rName <- G.getByteString lenghtName
  rType <- G.getWord8

  -- TODO Cache-Flush
  -- https://en.wikipedia.org/wiki/Multicast_DNS#Resource_Records
  rClass <- G.getWord8
  rTTL <- G.getInt16be
  rRDLength <- G.getInt16be

  --lengthData <- fromIntegral <$> G.getWord8
  --rRData <- G.getByteString lengthData
  pure $ Resource rName rType rClass rTTL rRDLength
