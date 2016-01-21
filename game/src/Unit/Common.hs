{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Unit.Common where

import Display
import System.Console.ANSI
import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent               
import Control.Monad
import Data
import Functions
import Stats
import Data.Time.Units
import Data.Array.IO
import Control.Monad.State
import Targeting.Preference
import Unit.Variety
import Data.Time.Clock.POSIX
import LensM
import Data.Either as Either
import Data.Either.Combinators
import Data.Easy
import Data.List 
import Unit.Type             
import Data.Monoid
import FieldAccess
import StatsLens
import Data.Default
          

newCasting :: Skill -> InterruptHandler -> IO Casting
newCasting skill interrupt  =  do
    let duration = skill^.cd
    start <- getPOSIXTime
    return $ Casting skill interrupt $ do   
        -- count fraction of cast progress, depending on access time
        cur <- getPOSIXTime     
        let durationMcs = fromIntegral $ toMicroseconds $ duration
        let res = fromIntegral (round $ (cur - start) * 10^6 :: Integer) / durationMcs :: Double
        return $ max 0 $ min 1 res

newDefUnit :: Side -> UnitTemplate -> Int -> IO Unit
newDefUnit s t i  =  do
    unitLog     <- createUnitLog (0 :: Second) ""
    return Unit { _unitType   = _unitTypeT t
                , _stats      = initStats $ _unitTypeT t
                , _side       = s
                , _casting    = Nothing 
                , _unitId     = i
                , _unitLog    = unitLog
                , _unitPrefer = _unitPreferT t
                }

defTemplate :: UnitType -> UnitTemplate 
defTemplate t  =  UnitTemplate
    { _unitTypeT = t
    , _unitPreferT = defTargetPrefer t
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





