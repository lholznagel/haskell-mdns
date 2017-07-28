module Network.MDNS.Internal where

import           Data.ByteString (ByteString)
import           Data.IP         (IP, IPv4, IPv6)
import           Data.Maybe      (fromMaybe)
import           Data.Word

-- | Type for domain.
--newtype Domain = Domain ByteString deriving (Eq, Show)
type Domain = ByteString

data Flags = Flags
  {
    qr     :: !Bool
  , opcode :: !Word8
  , aa     :: !Bool
  , tc     :: !Bool
  , rd     :: !Bool
  , ra     :: !Bool
  , rcode  :: !Word8
  } deriving (Show)

data Message = Message {
  header:: Header
  , question :: [Question]
  , answer :: [ResourceRecord]
  , authority :: [ResourceRecord]
  , additional :: [ResourceRecord]
}

data Header = Header {
  identifier :: Word16
  , flags    :: Flags
}

data Question = Question
  {
    questionName :: Domain
  , questionType :: TYPE
  } deriving (Eq, Show)

data ResourceRecord
    = ResourceRecord Domain TYPE Word32 RData
    | OptRecord Word16 Bool Word8 RData
    deriving (Show)

data RData = RD_NS Domain
           | RD_CNAME Domain
           | RD_DNAME Domain
           | RD_MX Word16 Domain
           | RD_PTR Domain
           | RD_SOA Domain Domain Word32 Word32 Word32 Word32 Word32
           | RD_A IPv4
           | RD_AAAA IPv6
           | RD_TXT ByteString
           | RD_SRV Word16 Word16 Word16 Domain
           | RD_OPT [OData]
           | RD_OTH ByteString
           | RD_TLSA Word8 Word8 Word8 ByteString
           | RD_DS Word16 Word8 Word8 ByteString
    deriving (Show)

data OData = OD_ClientSubnet Word8 Word8 IP
           | OD_Unknown Int ByteString
    deriving (Eq,Show,Ord)

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

-- https://www.iana.org/assignments/dns-parameters/dns-parameters.xhtml#dns-parameters-4
rrDB :: [(TYPE, Word16)]
rrDB = [
    (A,      1)
  , (NS,     2)
  , (CNAME,  5)
  , (SOA,    6)
  , (PTR,   12)
  , (MX,    15)
  , (TXT,   16)
  , (AAAA,  28)
  , (SRV,   33)
  , (DNAME, 39) -- RFC 6672
  , (OPT,   41) -- RFC 6891
  , (DS,    43) -- RFC 4034
  , (RRSIG, 46) -- RFC 4034
  , (NSEC,  47) -- RFC 4034
  , (DNSKEY, 48) -- RFC 4034
  , (NSEC3, 40) -- RFC 5155
  , (NSEC3PARAM, 51) -- RFC 5155
  , (TLSA,  52) -- RFC 6698
  , (CDS,   59) -- RFC 7344
  , (CDNSKEY, 60) -- RFC 7344
  , (CSYNC, 62) -- RFC 7477
  ]

intToType :: Word16 -> TYPE
intToType n = fromMaybe (UNKNOWN n) $ rookup n rrDB

typeToWord :: TYPE -> Word16
typeToWord (UNKNOWN x)  = x
typeToWord t = fromMaybe (error "typeToWord") $ lookup t rrDB

rookup :: (Eq b) => b -> [(a, b)] -> Maybe a
rookup _ [] = Nothing
rookup key ((x, y): xys)
  | key == y = Just x
  | otherwise = rookup key xys
