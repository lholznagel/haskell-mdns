module Network.MDNS.Internal where

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
  , resourceType     :: ByteString
  , resourceClass    :: ByteString
  , resourceTTL      :: Int32
  , resourceRDLength :: Int16
  , resourceRData    :: ByteString
  } deriving (Show)

data Header = Header
  {
    headerId :: !Word16
  , flag     :: !(Either String Flag)
  , qdCount  :: !Int16
  , anCount  :: !Int16
  , nsCount  :: !Int16
  , arCount  :: !Int16
  , question :: ![Question]
  , resource :: ![Resource]
  } deriving (Show)
