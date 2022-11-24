module LB.Backend(getBackEnd) where

import LB.Config (Backend (..), ServerPool (..))

-- Gets the next live backend
getBackEnd :: ServerPool -> Int -> Maybe Int
getBackEnd pool acc
    | alive $ serverPool pool !! currentServer pool = Just $ currentServer pool
    | acc == length (serverPool pool) = Nothing
    | otherwise = getBackEnd incServer (acc + 1)
  where
    serverIndex = currentServer pool
    poolSize = length $ serverPool pool
    incServer = pool{currentServer = (serverIndex + 1) `mod` poolSize}
