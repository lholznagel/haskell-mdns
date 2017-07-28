{-# LANGUAGE OverloadedStrings #-}

module DecodeTest ( tests ) where

import qualified Data.Binary.Get         as SG
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8   as BC

import           Control.Monad           (replicateM)
import           Data.IP                 (IPv4, toIPv4)
import           Data.Monoid             ((<>))
import           Data.Word               (Word8)
import           Network.MDNS.Decode
import           Network.MDNS.Encode
import           Network.MDNS.Internal
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck   as QC

getQuestion :: Question
getQuestion = Question "a.b" TXT

tests :: TestTree
tests = testGroup "Decode"
  [
    testCase "Finding out what the builder builds" $ assertEqual "" "" (BS.toLazyByteString $ questionBuilder getQuestion)
    , QC.testProperty "questionBuilder <-> questionParser" $ \msg -> msg === SG.runGet questionParser (BS.toLazyByteString $ questionBuilder msg)
  ]

instance Arbitrary Question where
  arbitrary = Question
    <$> genDomain
    <*> arbitrary

instance Arbitrary ResourceRecord where
  arbitrary = frequency
    [
      (8, genRR)
    ]
    where
      genRR = do
        dom <- genDomain
        t <- elements [ A ]
        ResourceRecord dom t <$> arbitrary <*> mkRData dom t

instance Arbitrary TYPE where
  arbitrary = frequency
    [ (20, elements
            [ A, AAAA, NS, TXT, MX, CNAME, SOA, PTR, SRV, DNAME, OPT, DS, RRSIG
            , NSEC, DNSKEY, NSEC3, NSEC3PARAM, TLSA, CDS, CDNSKEY, CSYNC
            ])
    , (1, intToType <$> arbitrary)
    ]

instance Arbitrary IPv4 where
  arbitrary = toIPv4 <$> replicateM 4 (fromIntegral <$> genWord8)

genByteString :: Gen BC.ByteString
genByteString = elements
    [ "", "a", "a.b", "abc", "a.b.c" ]

genDomain :: Gen Domain
genDomain = do
    bs <- genByteString
    pure $ bs <> "."

genWord8 :: Gen Word8
genWord8 = arbitrary

mkRData :: Domain -> TYPE -> Gen RData
mkRData dom typ =
    case typ of
        A  -> RD_A <$> arbitrary
        NS -> pure $ RD_NS dom
        _  -> pure . RD_TXT $ BC.pack "Unhandled type"
