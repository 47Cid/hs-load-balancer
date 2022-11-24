{-# LANGUAGE OverloadedStrings #-}

module LB.Proxy (
    run,
) where

import Control.Concurrent (MVar, forkFinally, forkIO, newMVar, putMVar, takeMVar)
import qualified Control.Exception as E
import Control.Monad (void)
import LB.Backend (getBackEnd)
import LB.Config (Backend (..), ServerPool (..), poolConfig)
import LB.HealthCheck (healthCheck)
import LB.Logger (logReq)
import Network.Socket
import Network.Socket.ByteString (recv, send)

handler :: Backend -> Socket -> IO ()
handler backend sock = do
    req <- recv sock 1024
    msg <- handle' req
    logReq backend req msg
    void $ send sock msg
  where
    handle' req = runTCPClient (ip backend) (port backend) $ \s -> do
        _ <- send s req
        recv s 1024

run :: IO ()
run = do
    sPool <- newMVar poolConfig
    _ <- forkIO $ healthCheck sPool
    runTCPServer Nothing "8000" handler sPool

runTCPServer :: Maybe HostName -> ServiceName -> (Backend -> Socket -> IO a) -> MVar ServerPool -> IO ()
runTCPServer mhost server_port server sPool = withSocketsDo $ do
    addr <- resolve

    E.bracket (open addr) close (loop (ServerPool{serverPool = [], currentServer = 0}))
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
    loop _pool sock = do
        (conn, _peer) <- accept sock

        --Accquire server pools
        pool <- takeMVar sPool

        -- Get a healthy server

        let index = getBackEnd pool 1

        case index of
            Just val -> do
                let backend = serverPool pool !! val
                    poolSize = length $ serverPool pool
                    newBackends = pool{currentServer = (val + 1) `mod` poolSize}

                --Release server pools
                _ <- putMVar sPool $! newBackends
                -- Handle the request on the different thread
                _ <- forkFinally (server backend conn) (const $ gracefulClose conn 5000)
                -- Start listening to clients again
                loop newBackends sock
            Nothing -> do
                putStrLn "Could not find server"

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
