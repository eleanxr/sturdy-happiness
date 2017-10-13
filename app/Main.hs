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

initAES256 :: BL.ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey

encrypt :: BL.ByteString -> BL.ByteString -> BL.ByteString
encrypt key message = ecbEncrypt (initAES256 (expandKey key)) (padMessage message)

main :: IO ()
-- main = print (decode testContent :: Maybe TestData)
main = print $ encrypt "2manysecrets" "Will Sucks"
