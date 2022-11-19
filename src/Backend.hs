module Backend where

data Backend = Backend {url :: String, alive :: Bool}

data ServerPool = ServerPool {server_pool :: [Backend], current_server :: Int}

x :: Backend
x = Backend{url = "hey", alive = False}
