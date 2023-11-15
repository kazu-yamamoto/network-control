module Network.Control.Rate (
    -- * Rate control
    Rate,
    newRate,
    getRate,
) where

import Data.IORef
import Data.UnixTime

-- | Type for rating.
newtype Rate = Rate (IORef Counter)

data Counter = Counter Int UnixTime

-- | Creating a new 'Rate'.
newRate :: IO Rate
newRate = do
    cntr <- Counter 0 <$> getUnixTime
    Rate <$> newIORef cntr

-- | Getting the current rate.
-- If one or more seconds have passed since the previous call, 1 is
--   returned. Otherwise, incremented counter number is returned.
getRate :: Rate -> IO Int
getRate (Rate ref) = do
    Counter n beg <- readIORef ref
    cur <- getUnixTime
    if (cur `diffUnixTime` beg) > 1
        then do
            let n' = 1
            writeIORef ref $ Counter n' cur
            return n'
        else do
            let n' = n + 1
            writeIORef ref $ Counter n' beg
            return n'
