{-# LANGUAGE ExistentialQuantification #-}

module Buff where

import Data
import Stats
import Data.Time.Clock.POSIX
import Data.Time.Units
import Control.Lens.TH
import Control.Concurrent
import Control.Monad
import Functions
import Control.Lens
import FieldAccess
import Control.Concurrent.STM
        
applyEffect :: Cooldown -> ExpireCond -> Action STM -> Action IO
applyEffect delay expired action caster target field  =  void $ forkIO $ do
    expired' <- expired target
    if expired'
        then return ()
    else do
        raiseIO $ action caster target field
        
        let intMaxValue  =  toInteger (maxBound :: Int)
        threadDelay $ fromIntegral $ min intMaxValue $ toMicroseconds delay
        applyEffect delay expired action caster target field

applyBuff :: IO Modifier -> Action IO
applyBuff modifier' caster target field  =  do
    modifier <- modifier'
    field & forUnit (target^.unitId).stats.modifiers %~ (:) modifier

permanent :: (ExpireCond -> Modifier) -> Modifier
permanent = ( $ neverExpires)

neverExpires :: ExpireCond
neverExpires _  =  return False

expiresIn :: TimeUnit t => t -> IO ExpireCond
expiresIn duration  =  do
    start <- getPOSIXTime
    return $ const $ do
        cur <- getPOSIXTime
        return $ (round $ (cur - start) * 10^6 :: Integer) > toMicroseconds duration

infixr 2 <@>
(<@>) :: ExpireCond -> ExpireCond -> ExpireCond
(a <@> b) t  =  any id <$> mapM ( $ t) [a, b]
                              
