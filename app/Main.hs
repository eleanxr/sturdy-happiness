{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Main where

import Lib

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString(ByteString)

import Crypto.Cipher

data TestData = TestData {
  foo :: Integer
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''TestData)

testContent :: BL.ByteString
testContent = "{ \"foo\": 42 }"

initAES256 :: ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey

encrypt :: ByteString -> ByteString -> ByteString
encrypt key message = ecbEncrypt (initAES256 key) message

main :: IO ()
-- main = print (decode testContent :: Maybe TestData)
main = print $ encrypt "2manysecretsxxxxxxxxxxxxxxxxxxxx" "Will Sucks xxxxx"
