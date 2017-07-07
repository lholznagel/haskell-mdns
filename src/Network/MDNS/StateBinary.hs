module Network.MDNS.StateBinary where

import qualified Control.Monad.State        as ST
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Types      as T
import qualified Data.IntMap                as IM

import           Control.Monad.State        (StateT)
import           Data.ByteString            (ByteString)
import           Data.IntMap                (IntMap)
import           Data.Word                  (Word16, Word8)
import           Network.MDNS.Types         (Domain)

type SGet = StateT PState (T.Parser ByteString)

data PState = PState {
    psDomain   :: IntMap Domain
  , psPosition :: Int
}

getPosition :: SGet Int
getPosition = psPosition <$> ST.get

addPosition :: Int -> SGet()
addPosition n = do
  PState domain position <- ST.get
  ST.put $ PState domain(position + n)

push :: Int -> Domain -> SGet ()
push n d = do
    PState domain position <- ST.get
    ST.put $ PState (IM.insert n d domain) position

pop :: Int -> SGet (Maybe Domain)
pop n = IM.lookup n . psDomain <$> ST.get

get8 :: SGet Word8
get8 = ST.lift A.anyWord8 <* addPosition 1

get16 :: SGet Word16
get16 = ST.lift getWord16be <* addPosition 2
  where
    word8' = fromIntegral <$> A.anyWord8
    getWord16be = do
        a <- word8'
        b <- word8'
        return $ a * 0x100 + b

getInt8 :: SGet Int
getInt8 = fromIntegral <$> getInt8

getNByteString :: Int -> SGet ByteString
getNByteString n = ST.lift (A.take n) <* addPosition n
