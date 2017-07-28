{-# LANGUAGE RecordWildCards #-}

module Network.MDNS.Encode where

import qualified Data.ByteString.Builder as BS

import           Data.Monoid             ((<>))
import           Network.MDNS.Internal

questionBuilder :: Question -> BS.Builder
questionBuilder Question{..} =
  BS.byteString questionName
  <> BS.word16BE (typeToWord questionType)
