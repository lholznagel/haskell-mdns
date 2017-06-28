module Lib ( response ) where

import           Network.Multicast
import           Network.Socket                 hiding (recv, recvFrom, send,
                                                 sendTo)
import           Network.Socket.ByteString.Lazy

import qualified Data.Binary.Get                as G
import           GHC.Int
import           HeaderParser

ip :: [Char]
ip = "224.0.0.251"

port :: PortNumber
port = 5353

maxByteLength :: Int64
maxByteLength = 1024

response :: IO a
response = do
  sock <- multicastReceiver ip port
  msg <- recv sock maxByteLength
  print msg
  let header = G.runGet parseHeader msg
  print header
  response
