module LB.HealthCheck(healthCheck) where

import Control.Concurrent (MVar, putMVar, takeMVar, threadDelay)
import Control.Exception (try)
import qualified Control.Exception as E
import LB.Config (Backend (..), ServerPool (..), poolConfig)
import Network.Socket

isActive :: Backend -> IO Backend
isActive server = withSocketsDo $ do
    addr <- resolve
    sock <- openSocket addr
    result <- try (connect sock $ addrAddress addr) :: IO (Either E.SomeException ())
    close sock
    return $ case result of
        Left _ -> server{alive = False}
        Right _ -> server{alive = True}
  where
    resolve = do
        let hints = defaultHints{addrSocketType = Stream}
        head <$> getAddrInfo (Just hints) (Just $ ip server) (Just $ port server)

updatePool :: ServerPool -> IO ServerPool
updatePool pool = do
    y <- mapM isActive (serverPool poolConfig)
    return pool{serverPool = y}

healthCheck :: MVar ServerPool -> IO ()
healthCheck poolVar = do
    pool <- takeMVar poolVar
    -- Update Server Pool
    newPool <- updatePool pool
    _ <- putMVar poolVar $! newPool
    -- threadDelay 5000000
    threadDelay 50000
    healthCheck poolVar
