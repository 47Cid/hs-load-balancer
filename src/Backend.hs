module Backend where

data Backend = Backend {ip :: String, port :: String, alive :: Bool}

data ServerPool = ServerPool {serverPool :: [Backend], currentServer :: Int}

test_pool :: ServerPool
test_pool =
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

getBackEnd :: ServerPool -> Int
getBackEnd pool = currentServer pool
