{-# LANGUAGE OverloadedStrings #-}
module TextOperations where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary.Put

padMessage :: ByteString -> ByteString
padMessage message = B.concat [message, B.replicate (16 - modulus) '\0']
  where modulus = (B.length message) `mod` 16

padString :: String -> ByteString
padString = padMessage . B.pack

padEcb :: ByteString -> ByteString
padEcb message = BL.toStrict $ runPut (serializeEcb message)

padEcbString :: String -> ByteString
padEcbString = padEcb . B.pack

serializeEcb :: ByteString -> Put
serializeEcb message = do
  putByteString message
  putByteString $ B.replicate (16 - modulus) '\0'
    where modulus = (B.length message `mod` 16)
