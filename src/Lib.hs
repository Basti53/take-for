module Lib
    ( takeFor
    ) where

import System.Timeout
import System.CPUTime

type Seconds = Integer

takeFor :: Seconds -> [a] -> IO [a]
takeFor n = takeFor' $ n * 10^12

takeFor' :: Integer -> [a] -> IO [a]
takeFor' _ [] = return []
takeFor' n (x:xs) = do
    time1 <- getCPUTime
    y <- timeout (fromIntegral $ n*10^6) $ x `seq` return x
    time2 <- getCPUTime
    let time = time2 - time1
    case y of
        Nothing -> return []
        Just x' -> fmap (x':) $ takeFor' (n-time) xs