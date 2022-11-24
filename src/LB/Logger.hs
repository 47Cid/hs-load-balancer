module LB.Logger (logReq) where

import qualified Data.ByteString.Char8 as C
import LB.Config (Backend (..), logFile)

logReq :: Backend -> C.ByteString -> C.ByteString -> IO ()
logReq backend req resp = do
    appendFile logFile $
        "\n\n"
            ++ ("Connected to: " ++ show backend)
            ++ "\n"
            ++ ("Request: " ++ show req)
            ++ "\n"
            ++ ("Respones: " ++ show resp)
            ++ "\n"
