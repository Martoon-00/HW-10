{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Modifiers where

import Stats
import Data
import Control.Lens
import LensM
import StatsLens
import Functions
import {-# SOURCE #-} Unit.Variety

-- modifier builder
type ModB  =  ExpireCond -> Modifier

healthMult :: Double -> ModB
healthMult coef  =  Modifier 10 $ liftIso hp $ from $ (#*) coef

incomingDamageMult :: Double -> ModB
incomingDamageMult coef  =  Modifier 10 $ lens id $ set
  where
    set s b 
        | s^.hp > b^.hp  =  b & hp -~ (ceiling $ ( * (1 - coef)) $ fromIntegral (b^.hp - s^.hp))
        | otherwise          =  b

manaShield :: Int -> ModB
manaShield coef = Modifier 10 $ lens id set
  where                         
    set _ unit  =  let absorbed = min (unit^.revStat hp) (unit^.mp * coef) in
        unit & mp -~ (absorbed + coef - 1 & (`div` coef)) 
             & hp +~ absorbed

(#*) :: Double -> Iso' Int Int
(#*) k  =  iso (inDoubles ( * k)) (inDoubles ( / k))
  where
    inDoubles :: (Double -> Double) -> Int -> Int
    inDoubles f  =  round . f . fromIntegral

                                                                            
