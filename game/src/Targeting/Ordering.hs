{-# LANGUAGE TupleSections #-}

module Targeting.Ordering where

import Control.Monad
import Data.Monoid
import Control.Lens
import Control.Monad.Trans
import Data.List

-- This monad accepts pack of items,
-- allowing to extract and process them in parts 
-- It's wrapper on Initial list -> ((Processed items, Remaining items), Currently processing items)
data OrderT l m a  =  OrderT { runOrderT :: l -> m ((l, l), a) }

buildOrderT :: (Monoid l, Monad m) => OrderT l m () -> l -> m l
buildOrderT (OrderT f) l  =  g <$> f l
  where
    g ((r, _), ())  =  r 


instance (Monoid l, Monad m) => Functor (OrderT l m) where
    fmap g (OrderT f)  =  OrderT $ (over _2 g <$>) . f

instance (Monoid l, Monad m) => Applicative (OrderT l m) where
    pure a   =  OrderT $ \l -> return ((mempty, l), a)
    -- (<*>) f  =  (=<<) $ ( <$> f) . (&)  
    (<*>)  =  ap


instance (Monoid l, Monad m) => Monad (OrderT l m) where
    return  =  pure
    (OrderT m) >>= g  =  OrderT $ f
      where
        f items  =  do
            ((sorted1, remain1), v1)  <-  m items
            let (OrderT m2)           =   g v1
            ((sorted2, remain2), v2)  <-  m2 remain1
            return ((sorted1 <> sorted2, remain2), v2)

instance Monoid l => MonadTrans (OrderT l) where
    lift m  =  OrderT $ \l -> ((mempty, l), ) <$> m

instance (Monoid l, MonadIO m) => MonadIO (OrderT l m) where
    liftIO  =  lift . liftIO

-- extracts desired items from "remaining" heap
extract :: (Monad m) => (e -> Bool) -> OrderT [e] m [e]   -- TODO: think of replacing [] with Traversable or smth similar
extract p  =  OrderT $ \l -> 
    let (good, bad)  =  foldMap (partition p . pure) l in
        return ((mempty, bad), good)
    
-- puts specified items to "ready" heap
push :: (Monoid l, Monad m) => l -> OrderT l m ()
push v  =  OrderT $ \l -> return ((v, l), ())
    
