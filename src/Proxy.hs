{-# LANGUAGE OverloadedStrings #-}

module Proxy (
    someFunc,
) where

import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Network.Simple.TCP

con :: B.ByteString -> IO (Maybe B.ByteString)
con msg = connect "localhost" "3000" $ \(connectionSocket, remoteAddr) -> do
    send connectionSocket msg
    recv connectionSocket 1024

handle :: IO ()
handle = serve (Host "127.0.0.1") "8000" $ \(connectionSocket, remoteAddr) -> do
    msg <- recv connectionSocket 1024
    print $ "Recieved: " ++ show (fromMaybe "" msg)
    output <- con $ fromMaybe "" msg
    print $ "Recieved: " ++ show (fromMaybe "" output)
    send connectionSocket $ fromMaybe "" output

someFunc :: IO ()
someFunc = handle
