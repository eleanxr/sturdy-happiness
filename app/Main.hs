{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Main where

import KeyOperations
import TextOperations

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BL
import Data.ByteString.Char8(ByteString)

import Crypto.Cipher

data TestData = TestData {
  foo :: Integer
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestData)

testContent :: BL.ByteString
testContent = "{ \"foo\": 42 }"

testData = TestData 42

initAES256 :: BL.ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey . expandKey

encrypt :: BL.ByteString -> BL.ByteString -> BL.ByteString
encrypt key message = ecbEncrypt (initAES256 key) (padMessage message)

decrypt :: BL.ByteString -> BL.ByteString -> BL.ByteString
decrypt key message = ecbDecrypt (initAES256 key) message

key = "2manysecrets"
message = "This is a test message"

main :: IO ()
main = do
  ciphertext <- return $ encrypt key message
  print ciphertext
  print $ decrypt key ciphertext
