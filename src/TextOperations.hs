{-# LANGUAGE OverloadedStrings #-}
module TextOperations where

import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B

padMessage :: ByteString -> ByteString
padMessage message = B.concat [message, B.replicate (16 - modulus) '\0']
  where modulus = (B.length message) `mod` 16

padString :: String -> ByteString
padString = padMessage . B.pack
