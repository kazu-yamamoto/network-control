{-# LANGUAGE RecordWildCards #-}

module Network.Control.LRUCache (
    -- * LRU cache
    LRUCache,
    empty,
    insert,
    delete,
    lookup,
) where

import Prelude hiding (lookup)

import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ

type Priority = Int

-- | Sized cache based on least recently used.
data LRUCache k v = LRUCache
    { lcLimit :: Int
    , lcSize :: Int
    , lcTick :: Priority
    , lcQueue :: OrdPSQ k Priority v
    }

-- | Empty 'LRUCache'.
empty
    :: Int
    -- ^ The size of 'LRUCache'.
    -> LRUCache k v
empty lim = LRUCache lim 0 0 PSQ.empty

-- | Inserting.
insert :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
insert k v c@LRUCache{..}
    | lcSize == lcLimit =
        let q = PSQ.insert k lcTick v $ PSQ.deleteMin lcQueue
         in c{lcTick = lcTick + 1, lcQueue = q}
    | otherwise =
        let q = PSQ.insert k lcTick v lcQueue
         in c{lcTick = lcTick + 1, lcQueue = q, lcSize = lcSize + 1}

-- | Deleting.
delete :: Ord k => k -> LRUCache k v -> LRUCache k v
delete k c@LRUCache{..} =
    let q = PSQ.delete k lcQueue
     in c{lcQueue = q, lcSize = lcSize - 1}

-- | Looking up.
lookup :: Ord k => k -> LRUCache k v -> Maybe v
lookup k LRUCache{..} = snd <$> PSQ.lookup k lcQueue
