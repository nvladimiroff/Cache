{-# LANGUAGE RankNTypes, ExistentialQuantification, ScopedTypeVariables #-}
-- | A simple and threadsafe in-memory cache

-- TODO: Does Cache have any other typeclass instances?
--       Can IO be factored out, and will factoring it out have significants benefits?
module Cache (Cache, CacheMap, newCache, newCacheWith, lookup, createLookup) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Concurrent.MVar
import Control.Exception (catch, throw, SomeException(..))
import qualified Data.Map as M

-- | An in-memory cache with a cache miss function to generate values if a key is not
-- stored locally. Operationally, this is a Map that runs in IO and has lookup that will
-- always return a value of type @val@.
-- Kept threadsafe by using an MVar.
data Cache map key val = forall x. Cache (key -> IO x) (x -> val) (MVar (map key x))

-- | A convience alias for a Cache that uses 'Data.Map.Map' as its internal structure.
type CacheMap = Cache M.Map

instance Functor (Cache map key) where
    fmap f (Cache miss extr mvar) = Cache miss (f . extr) mvar

-- | Creates a new cache by providing a function to use in case of a cache miss.
-- Uses Data.Map as its internal structure.
newCache :: (key -> IO val) -> IO (CacheMap key val)
newCache f = newCacheWith M.empty

-- | Creates a new cache by providing a cache miss function as well as an initial cache.
newCacheWith :: map key val -> (key -> IO val) -> IO (Cache map key val)
newCacheWith initial f = Cache f id <$> newMVar initial

-- | Loads a value from a CacheMap. If the value is not found, the cache miss function is
-- used to generate a new value and adds this to the cache.
lookup :: Ord key => key -> CacheMap key val -> IO val
lookup = createLookup M.lookup M.insert

-- | Given a lookup and insert function, this will create a new lookup function specialized to @Cache@.
-- This can be used to create new caches that don't depend on the internal cache structure.
-- For instance:
--
-- @
-- lookup :: Ord key => key -> Cache M.Map key val -> IO val
-- lookup = createLookup M.lookup M.insert
-- @
--
-- This will give you a 'lookup' function that is specialized to Caches dealing with 'Data.Map.Map'.
createLookup
    :: (forall val'. key -> map key val' -> Maybe val') -- ^ The lookup function
    -> (forall val'. key -> val' -> map key val' -> map key val') -- ^ The insert function
    -> (key -> Cache map key val -> IO val) -- ^ The specialized lookup function
createLookup lookup' insert' =
    \key (Cache miss extr mvar) -> do
        dict <- takeMVar mvar
        case lookup' key dict of
            Just item -> do
                putMVar mvar dict
                return (extr item)
            Nothing   -> do
                item <- miss key `catch` \(e :: SomeException) -> putMVar mvar dict >> throw e
                putMVar mvar (insert' key item dict)
                return (extr item)
