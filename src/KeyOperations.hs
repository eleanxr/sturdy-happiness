{-# LANGUAGE OverloadedStrings #-}
module KeyOperations where

import qualified Data.ByteString.Char8 as BL
import Data.ByteString.Char8(ByteString)
import Crypto.Hash.SHA256 as SHA256

expandKey :: ByteString -> ByteString
expandKey = SHA256.hash
