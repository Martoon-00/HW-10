{-# LANGUAGE TupleSections #-}

module Targeting.Ordering where

import Control.Monad
import Data.Monoid
import Control.Lens
import Control.Monad.Trans
import Data.List

data OrderT l m a  =  OrderT { runOrderT :: l -> m ((l, l), a) }

buildOrderT :: (Monoid l, Monad m) => OrderT l m () -> l -> m l

instance (Monoid l, Monad m) => Functor (OrderT l m) 
instance (Monoid l, Monad m) => Applicative (OrderT l m) 
instance (Monoid l, Monad m) => Monad (OrderT l m) 
instance Monoid l => MonadTrans (OrderT l) 
instance (Monoid l, MonadIO m) => MonadIO (OrderT l m) 

extract :: (Monad m) => (e -> Bool) -> OrderT [e] m [e]  
    
push :: (Monoid l, Monad m) => l -> OrderT l m ()
    

