{-# LANGUAGE InstanceSigs, RankNTypes #-}

module Targeting.Preference where

import Data
import Data.Bifunctor
import Data.Monoid
import Data.List
import Control.Lens
import Data.Tuple
import Random
import Targeting.Ordering
import Control.Monad.Reader
import Data.Function.Pointless
import Data.Function
import Data.Default

data ListSortPostProcess  =  SavePostProcess  
                          |  RevertPostProcess
                          |  RandomShufflePostProcess
                                                               
buildOrderPref :: OrderedPref -> [Unit] -> IO [Unit]
buildOrderPref RandomPref                          =  randomShuffle 
buildOrderPref (OrderedPref cmpPref prefCategory)  =  return . modifyRes cmpPref . sortBy (compare `on` (^. prefCategory)) 
  where
    modifyRes :: CmpPref -> [Unit] -> [Unit]
    modifyRes Lowest   =  id
    modifyRes Highest  =  reverse

buildTargetPrefer :: TargetPrefer -> LockedTargets -> [Unit] -> IO [Unit]
buildTargetPrefer  =  buildOrderT .: runReaderT

buildTargetPreferWay :: TargetPreferWay -> TargetPrefer                                                       
buildTargetPreferWay (PreferId filterUnitId)              =  lift $ extract (( == filterUnitId) . _unitId) >>= push
buildTargetPreferWay PreferLocked                         =  do
    locked <- ask
    lift $ extract (( `elem` locked) . _unitId) >>= push

buildTargetPreferWay (PreferType neededUnitType orderPref)  =  do
    items <- lift $ extract $ ( == neededUnitType) . _unitType
    sorted <- liftIO $ buildOrderPref orderPref items
    lift $ push sorted

buildTargetPreferWay (PreferNoGroup orderPref)            =  do
    items <- lift $ extract (const True)
    sorted <- liftIO $ buildOrderPref orderPref items
    lift $ push sorted

buildTargetPreferWay NoNextTarget                         =  do
    lift $ void $ extract $ const True


