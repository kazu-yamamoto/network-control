{-# LANGUAGE RecordWildCards #-}

module Network.Control.LRUCache (
    -- * LRU cache
    LRUCache,
    empty,
    insert,
    delete,
    lookup,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Prelude hiding (lookup)

type Priority = Int64

-- | Sized cache based on least recently used.
data LRUCache k v = LRUCache
    { lcLimit :: Int
    -- ^ The maximum number of elements in the queue
    , lcSize :: Int
    -- ^ The current number of elements in the queue
    , lcTick :: Priority
    -- ^ The next logical time
    , lcQueue :: OrdPSQ k Priority v
    }
    deriving (Eq, Show)

-- | Empty 'LRUCache'.
empty
    :: Int
    -- ^ The size of 'LRUCache'.
    -> LRUCache k v
empty capacity
    | capacity < 1 = error "LRUCache.empty: capacity < 1"
    | otherwise =
        LRUCache
            { lcLimit = capacity
            , lcSize = 0
            , lcTick = 0
            , lcQueue = PSQ.empty
            }

trim :: Ord k => LRUCache k v -> LRUCache k v
trim c@LRUCache{..}
    | lcTick == maxBound = empty lcLimit
    | lcSize > lcLimit =
        c
            { lcSize = lcSize - 1
            , lcQueue = PSQ.deleteMin lcQueue
            }
    | otherwise = c

-- | Inserting.
insert :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
insert key val c@LRUCache{..} =
    trim $
        let (mbOldVal, queue) = PSQ.insertView key lcTick val lcQueue
         in c
                { lcSize = if isNothing mbOldVal then lcSize + 1 else lcSize
                , lcTick = lcTick + 1
                , lcQueue = queue
                }

-- | Deleting.
delete :: Ord k => k -> LRUCache k v -> LRUCache k v
delete k c@LRUCache{..} =
    let q = PSQ.delete k lcQueue
     in c{lcQueue = q, lcSize = lcSize - 1}

-- | Looking up.
lookup :: Ord k => k -> LRUCache k v -> Maybe (v, LRUCache k v)
lookup k c@LRUCache{..} = case PSQ.alter lookupAndBump k lcQueue of
    (Nothing, _) -> Nothing
    (Just x, q) ->
        let c' = trim $ c{lcTick = lcTick + 1, lcQueue = q}
         in Just (x, c')
  where
    lookupAndBump Nothing = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x, Just (lcTick, x))

newtype Handle k v = Handle (IORef (LRUCache k v))

newHandle :: Int -> IO (Handle k v)
newHandle capacity = Handle <$> newIORef (empty capacity)

cached :: Ord k => Handle k v -> k -> IO v -> IO v
cached (Handle ref) k io = do
    lookupRes <- atomicModifyIORef' ref $ \c -> case lookup k c of
        Nothing -> (c, Nothing)
        Just (v, c') -> (c', Just v)
    case lookupRes of
        Just v -> return v
        Nothing -> do
            v <- io
            atomicModifyIORef' ref $ \c -> (insert k v c, ())
            return v
