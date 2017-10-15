{-# LANGUAGE OverloadedStrings #-}
module TextOperations where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Binary.Get
import Data.Binary.Put
import Data.Int

padMessage :: ByteString -> ByteString
padMessage message = B.concat [message, B.replicate (16 - modulus) '\0']
  where modulus = (B.length message) `mod` 16

padString :: String -> ByteString
padString = padMessage . B.pack

padEcb :: ByteString -> ByteString
padEcb message = BL.toStrict $ runPut (serializeEcb message)

unpadEcb :: ByteString -> ByteString
unpadEcb message = runGet (deserializeEcb (B.length message)) (BL.fromStrict message)

padEcbString :: String -> ByteString
padEcbString = padEcb . B.pack

unpadEcbString :: String -> ByteString
unpadEcbString = unpadEcb . B.pack

serializeEcb :: ByteString -> Put
serializeEcb message = do
  putByteString message
  putByteString $ B.replicate ((16 - modulus) - 4) '\0'
  putInt32be (fromIntegral (B.length message))
    where modulus = (B.length message `mod` 16)

deserializeEcb :: Int -> Get ByteString
deserializeEcb stringLength = do
  paddedMessage <- getByteString (stringLength - 4)
  plaintextLength <- getInt32be
  return $ B.take (fromIntegral plaintextLength) paddedMessage
