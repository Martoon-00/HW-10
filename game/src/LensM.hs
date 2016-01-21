{-# LANGUAGE RankNTypes, ScopedTypeVariables  #-}      
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}

module LensM where

import Control.Concurrent.MVar
import Control.Lens


type LensF f s t a b = Lens (f s) (f t) (f a) (f b)

type LensF' f s a = Lens' (f s) (f a)

type ALensF' f s a = ALens' (f s) (f a)

type GetterF f s a = Getter (f s) (f a)

type SetterF f s t a b = Setter (f s) (f t) (f a) (f b) 

type LensM m s t a b = Lens s (m t) a (m b)

type LensM' m s a = LensM m s s a a

                                                       
type LensLikeM f m s t a b = LensLike f s (m t) a (m b)

type LensLikeM' f m s a = LensLike f s (m s) a (m a)


setMVar :: IO (MVar a) -> IO a -> IO (MVar a)
setMVar ioVar ioValue = do
    var <- ioVar
    value <- ioValue
    swapMVar var value
    return var

getMVar :: IO (MVar a) -> IO a
getMVar ioVar = do
    var <- ioVar
    readMVar var
-- (flip (>>=)) readMVar 

mvarLens :: LensF' IO (MVar a) a
mvarLens = lens getMVar setMVar 


liftLensM :: Monad m => Lens' s a -> LensF' m s a
liftLensM pureLens = lens getM setM
    where
        getM mS = do
            s <- mS
            return (s^.pureLens)
        setM mS mValue = do
            s <- mS
            value <- mValue
            return (set pureLens value s)

lensM :: (Functor f, Monad m) => LensLike f a (m b) a b
lensM mapper  =  fmap return . mapper


liftIso :: Lens s s a a -> Iso' a a -> Iso' s s
liftIso l i  =  iso (up i) (up $ from i)
  where
    up i  =  over l $ withIso i const

bounding :: Int -> Int -> Lens' Int Int
bounding mini maxi  =  lens id $ const $ max mini . min maxi

