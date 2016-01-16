{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}

module FieldAccess where

import Control.Concurrent.MVar
import Control.Lens
import Data.Function
import Data.Array.IO
import Data.Array
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import UnitTypes
import LensM
import Data.Monoid
import Data                         
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Functions
import qualified Data.Array.IArray as A
                                         
class Monad m => AccessibleInto m where
    raiseIO :: m a -> IO a        

    fieldUnits :: Field -> m [Unit]
    
    unitById :: Field -> UnitId -> m Unit
    
    forUnit :: UnitId -> Setter Field (m ()) Unit Unit

instance AccessibleInto STM where
    raiseIO  =  atomically
 
    fieldUnits  =  mapM readTVar . A.elems . units 

    unitById field uid  =  readTVar $ units field A.! uid

    forUnit uid  =  sets $ \mapper field -> 
        let !unitTVar = units field A.! uid 
        in mapper <$> readTVar unitTVar >>= writeTVar unitTVar

instance AccessibleInto IO where
    raiseIO  =  id    

    fieldUnits  =  atomically . fieldUnits
    
    unitById  =  atomically .: unitById

    forUnit uid  =  (\f s -> atomically <$> f s) . forUnit uid 
    

