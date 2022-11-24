module LB.Config where

data Backend = Backend {ip :: !String, port :: !String, alive :: !Bool}
    deriving (Show, Eq)

data ServerPool = ServerPool {serverPool :: ![Backend], currentServer :: !Int}
    deriving (Show, Eq)

logFile :: String
logFile = "logs/logs.txt"

poolConfig :: ServerPool
poolConfig =
    ServerPool
        { serverPool =
            [ Backend{ip = "127.0.0.1", port = "3000", alive = True}
            , Backend{ip = "127.0.0.1", port = "3001", alive = True}
            , Backend{ip = "127.0.0.1", port = "3002", alive = True}
            , Backend{ip = "127.0.0.1", port = "3003", alive = True}
            , Backend{ip = "127.0.0.1", port = "3004", alive = True}
            ]
        , currentServer = 0
        }
