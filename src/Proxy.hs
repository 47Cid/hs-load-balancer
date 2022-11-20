{-# LANGUAGE OverloadedStrings #-}

module Proxy (
    someFunc,
) where

import Control.Concurrent (forkFinally, newEmptyMVar)
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (recv, send)

import Backend
import Control.Monad (void)

handler :: Backend -> Socket -> IO ()
handler backend sock = do
    req <- recv sock 1024
    msg <- handle' req
    void $ send sock msg
  where
    handle' req = runTCPClient (ip backend) (port backend) $ \s -> do
        _ <- send s req
        recv s 1024

someFunc :: IO ()
someFunc = do
    mVar <- newEmptyMVar
    runTCPServer Nothing "8000" handler

runTCPServer :: Maybe HostName -> ServiceName -> (Backend -> Socket -> IO a) -> IO a
runTCPServer mhost server_port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close (loop test_pool)
  where
    resolve = do
        let hints =
                defaultHints
                    { addrFlags = [AI_PASSIVE]
                    , addrSocketType = Stream
                    }
        head <$> getAddrInfo (Just hints) mhost (Just server_port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        return sock
    loop pool sock = do
        (conn, _peer) <- accept sock
        let index = getBackEnd pool
            backend = serverPool pool !! index
        _ <- forkFinally (server backend conn) (const $ gracefulClose conn 5000)
        loop pool{currentServer = (index + 1) `mod` poolSize} sock
      where
        poolSize = length $ serverPool pool

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host client_port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints{addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just host) (Just client_port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
