module Main where

import qualified DecodeTest

import           Test.Tasty

main :: IO ()
main =
  defaultMain $ testGroup "Network.MDNS"
    [
      DecodeTest.tests
    ]
