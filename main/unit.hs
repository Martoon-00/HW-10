{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Unit where

import Display
import System.Console.ANSI
import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent               
import Control.Monad
import Data
import Skill
import Functions
import Stats
import Data.Time.Units
import Data.Array.IO
import Control.Monad.State
import Preference
import Units
import Data.Time.Clock.POSIX
import LensM
import Data.Either as Either
import Data.Either.Combinators
import Data.Easy
import Data.List 
import UnitTypes             
import Data.Monoid
import FieldAccess
import StatsLens
          

newCasting :: TimeUnit t => t -> IO Casting
newCasting duration  =  do
    start <- getPOSIXTime
    return $ Casting $ do   -- count fraction of cast progress, depending on access time
        cur <- getPOSIXTime     
        let durationMcs = fromIntegral $ toMicroseconds $ duration
        let res = fromIntegral (round $ (cur - start) * 10^6 :: Integer) / durationMcs :: Double
        return $ max 0 $ min 1 res

noCasting :: IO Casting
noCasting  =  newCasting (99 :: Week)

newDefUnit :: Side -> UnitType -> Int -> IO Unit
newDefUnit s t i  =  do
    initCasting <- noCasting
    unitLog     <- createUnitLog (0 :: Second) ""
    return Unit { _unitType = t
                , _stats    = initStats t
                , _side     = s
                , _casting  = initCasting
                , _unitId   = i
                , _unitLog  = unitLog
                } 
                                             
oppositeSide :: Side -> Side       
oppositeSide LeftSide   =  RightSide
oppositeSide RightSide  =  LeftSide
    
createUnitLog :: TimeUnit t => t -> String -> IO UnitLog
createUnitLog duration text  =  do
    start <- getPOSIXTime
    let durationMcs = fromIntegral $ toMicroseconds $ duration
    
    return $ UnitLog $ do
        cur <- getPOSIXTime
        let expired = (round $ (cur - start) * 10^6 :: Integer) < durationMcs
        return $ if expired
            then text
            else ""
        
updateModifiers :: Unit -> IO Unit
updateModifiers unit  =  return unit 
                      &  liftLensM (stats.modifiers) 
                      %~ (>>= filterM (fmap not . (&) unit . _modifierExpired))
                                                       
instance Display Unit where
    display  =  sequence_ . flip map actions . (&)
      where
        actions = [\u -> if u^.hp <= 0 
                        then setSGR [SetColor Foreground Vivid Black]
                        else return ()
                  , const $ putStr "#"                               
                  ,         putStr . extendEnd 3 ' ' . show . _unitId
                  ,         putStr . extendEnd 16 ' ' . show . _unitType                        
                  , const $ setSGR []
                  , const $ putStr " "
                  ,         display . _stats
                  ]
                                                   
                                                           
instance Show Unit where
    show  =  show . _unitType



