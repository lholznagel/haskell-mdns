{-# LANGUAGE OverloadedStrings #-}

module Network.MDNS.Decoder where

import qualified Data.Binary.Get           as SG
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString.Char8     as BS

import           Control.Monad
import           Data.Bits                 (testBit, (.&.))
import           Data.Int
import           Network.MDNS.Internal
import           Network.MDNS.StateBinary

data ResourceRecord = ResourceRecord {
  domain :: Domain
}

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

getResourceRecords :: Int -> SG.Get [Resource]
getResourceRecords count' = replicateM count' parseResource

-------- NEW STUFF --------
getResourceRecord :: SGet ResourceRecord
getResourceRecord = do
  domain' <- getDomain
  typ' <- getTYPE
  pure $ ResourceRecord domain'

getTYPE :: SGet TYPE
getTYPE = intToType <$> get16

getDomain :: SGet Domain
getDomain = do
  position' <- getPosition
  c <- getInt8
  let n = getValue c

  -- syntax hack to avoid using MultiWayIf
  case () of
    _ | c == 0 -> return "."
    _ | isPointer c -> do
      d <- getInt8
      let offset = n * 256 + d
      mo <- pop offset
      case mo of
        Nothing -> fail $ "getDomain: " ++ show offset
        -- A pointer may refer to another pointer.
        -- So, register the position for the domain
        Just o  -> push position' o >> return o
    -- As for now extended labels have no use
    -- This may change in the future
    _ | isExtLabel c -> return ""
    _ -> do
      hs <- getNByteString n
      ds <- getDomain
      let domain' =
                  case ds of
                    "." -> hs `BS.append` "."
                    _   -> hs `BS.append` "." `BS.append` ds
      push position' domain'
      return domain'
  where
    getValue c = c .&. 0x3f
    isPointer c = testBit c 7 && testBit c 6
    isExtLabel c = not (testBit c 7) && testBit c 6
