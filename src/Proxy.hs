{-# LANGUAGE OverloadedStrings #-}

module Proxy (
    someFunc,
) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (recv, send)

handler :: Socket -> IO ()
handler sock = do
    req <- recv sock 1024
    msg <- handle' req
    _ <- send sock msg
    putStr "Request Handled\n"
  where
    handle' req = runTCPClient "127.0.0.1" "3000" $ \s -> do
        _ <- send s req
        recv s 1024

someFunc :: IO ()
someFunc = runTCPServer Nothing "8000" handler

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints{addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop sock = do
        (conn, _peer) <- accept sock
        _ <- forkFinally (server conn) (const $ gracefulClose conn 5000)
        loop sock
