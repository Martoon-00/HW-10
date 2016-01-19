{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types #-}

module Stats where

import System.Console.ANSI
import Data.List
import Data.Time.Units
import Display
import Control.Lens
import Data
import LensM
                             
instance Stat HitPoints Int where
    statValue (HP k)          =  k
    statWrap                  =  HP
    statName  _               =  "HP"
    statColor _               =  Red 
    statColorIntensity (HP k)
        | k > 0               =  Vivid
        | otherwise           =  Dull
    statMaxDigits _           =  4

instance Stat ManaPoints Int where
    statValue (MP k)          =  k
    statWrap                  =  MP
    statName  _               =  "MP"
    statColor _               =  Cyan
--    statColorIntensityExt stats (MP k)
--        | k >= statValue (_mc stats)  =  Vivid
--        | otherwise                   =  Dull
    statMaxDigits _           =  4

instance Stat DamageValue Int where
    statValue (Dmg k)         =  k
    statWrap                  =  Dmg
    statName  _               =  "Eff"
    statColor _               =  Magenta
    statColorIntensity _      =  Dull
    statMaxDigits _           =  3

instance Stat Cooldown Int where
    statValue (CD k)          =  fromInteger k
    statWrap                  =  CD . toInteger
    statName  _               =  "CD"
    statColor _               =  Yellow
    statMaxDigits _           =  4

instance Stat ManaConsumption Int where
    statValue (MC k)          =  k
    statWrap                  =  MC
    statName  _               =  "MC"
    statColor _               =  Cyan
    statColorIntensity _      =  Dull
    statMaxDigits _           =  3

instance Stat Visibility Bool where
    statValue (Vis k)         =  k
    statWrap                  =  Vis
    statName  _               =  "Visibility"
    statColor _               =  Blue
    statColorIntensity _      =  Dull
    statMaxDigits _           =  9

instance TimeUnit Cooldown where
    toMicroseconds (CD t)  =  t * ticCD
    fromMicroseconds t     =  CD $ t `div` ticCD

ticCD :: Integer
ticCD  =  100000

instance Display Stats where
    display st  =  do     
        printStatExt st $ _hp st
        putStr delim
        printStatExt st $ _mp st   
      where
        delim  =  replicate 4 ' '


