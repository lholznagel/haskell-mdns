module Network.MDNS.Internal where

import           Data.ByteString
import           Data.Int
import           Data.Word

-- | Type for a domain
type Domain = ByteString

-- | Types for resource records.
data TYPE = A
          | AAAA
          | NS
          | TXT
          | MX
          | CNAME
          | SOA
          | PTR
          | SRV
          | DNAME
          | OPT
          | DS
          | RRSIG
          | NSEC
          | DNSKEY
          | NSEC3
          | NSEC3PARAM
          | TLSA
          | CDS
          | CDNSKEY
          | CSYNC
          | UNKNOWN Word16
          deriving (Eq, Show, Read)

data Flag = Flag
  {
    qr     :: !Bool
  , opcode :: !Word8
  , aa     :: !Bool
  , tc     :: !Bool
  , rd     :: !Bool
  , ra     :: !Bool
  , rcode  :: !Word8
  } deriving (Eq, Show, Read)

data Question = Question
  {
    questionName  :: ByteString
  , questionType  :: ByteString
  , questionClass :: ByteString
  } deriving (Eq, Show, Read)

data Resource = Resource
  {
    resourceName     :: ByteString
  , resourceType     :: ByteString
  , resourceClass    :: ByteString
  , resourceTTL      :: Int32
  , resourceRDLength :: Int16
  , resourceRData    :: ByteString
  } deriving (Eq, Show, Read)

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
  } deriving (Eq, Show, Read)
