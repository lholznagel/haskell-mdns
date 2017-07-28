{-# LANGUAGE OverloadedStrings #-}

module Network.MDNS.Decode (
  questionParser
) where

import qualified Data.Binary.Get           as SG
import qualified Data.Binary.Strict.BitGet as BG
import qualified Data.ByteString           as BS

import           Control.Monad             (replicateM)
import           Data.IP                   (toIPv4)
import           Network.MDNS.Internal

parseFlag :: BG.BitGet Flags
parseFlag = Flags
  <$> BG.getBit
  <*> BG.getAsWord8 4
  <*> BG.getBit
  <*> BG.getBit
  <*> BG.getBit
  <*> BG.getBit
  <*> (BG.getAsWord8 3 >> BG.getAsWord8 4) -- field z is for future use, so we skip it

getQuestion :: Int -> SG.Get [Question]
getQuestion count' = replicateM count' questionParser

questionParser :: SG.Get Question
questionParser = Question
  <$> getDomain
  <*> getTYPE
  <* ignoreClass

getResourceRecords :: Int -> SG.Get [ResourceRecord]
getResourceRecords count' = replicateM count' getResourceRecord

getResourceRecord :: SG.Get ResourceRecord
getResourceRecord = do
    dom <- getDomain
    typ <- getTYPE
    getRR dom typ
  where
    getRR dom typ = do
      ignoreClass
      ttl <- decodeTTL
      len <- decodeRLen
      dat <- getRData typ len
      return $ ResourceRecord dom typ ttl dat

    decodeTTL = SG.getWord32be
    decodeRLen = fromIntegral <$> SG.getInt16be

getRData :: TYPE -> Int -> SG.Get RData
getRData NS _ = RD_NS <$> getDomain
getRData A len
  | len == 4 = (RD_A . toIPv4) <$> getNBytes len
  | otherwise = fail "IPv4 addresses must be 4 bytes long"
getRData _ _ = fail "Not implemented"

getDomain :: SG.Get Domain
getDomain = do
  lenghtName <- fromIntegral <$> SG.getWord8
  SG.getByteString lenghtName

-- getDomain :: SG.Get Domain
-- getDomain = do
--   c <- getInt8
--   let n = getValue c
--
--   case () of
--     _ | c == 0 -> return "."
--     _ -> do
--       hs <- SG.getByteString n
--       ds <- getDomain
--       let dom =
--             case ds of
--               "." -> hs `BS.append` "."
--               _   -> hs `BS.append` "." `BS.append` ds
--       return dom
--
--   where
--     getValue c = c .&. 0x3f

getTYPE :: SG.Get TYPE
getTYPE = intToType <$> SG.getWord16be

getNBytes :: Int -> SG.Get [Int]
getNBytes len = toInts <$> SG.getByteString len
  where
    toInts = map fromIntegral . BS.unpack

getInt8 :: SG.Get Int
getInt8 = fromIntegral <$> SG.getInt8

ignoreClass :: SG.Get ()
ignoreClass = () <$ SG.getWord16be
