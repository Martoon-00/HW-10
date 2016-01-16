{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Modifiers where

import Stats
import Data
import Control.Lens
import LensM
import StatsLens
import Functions
import {-# SOURCE #-} Units

-- modifier builder
type ModB  =  ExpireCond -> Modifier

healthMult :: Double -> ModB
healthMult coef  =  Modifier 10 $ liftIso m_hp $ from $ (#*) coef

incomingDamageMult :: Double -> ModB
incomingDamageMult coef  =  Modifier 10 $ lens id $ set
  where
    set s b 
        | s^.m_hp > b^.m_hp  =  b & m_hp -~ (ceiling $ ( * (1 - coef)) $ fromIntegral (b^.m_hp - s^.m_hp))
        | otherwise          =  b

manaShield :: Int -> ModB
manaShield coef = Modifier 10 $ lens id set
  where                         
    set _ unit  =  let absorbed = min (unit^.revStat m_hp) (unit^.m_mp * coef) in
        unit & m_mp -~ (absorbed + coef - 1 & (`div` coef)) 
             & m_hp +~ absorbed

(#*) :: Double -> Iso' Int Int
(#*) k  =  iso (inDoubles ( * k)) (inDoubles ( / k))
  where
    inDoubles :: (Double -> Double) -> Int -> Int
    inDoubles f  =  round . f . fromIntegral

                                                                            
