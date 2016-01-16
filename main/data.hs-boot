{-# LANGUAGE TemplateHaskell, RankNTypes, ExistentialQuantification #-}

module Data where

import Control.Concurrent.MVar
import Control.Lens
import System.Console.ANSI
import {-# SOURCE #-} Functions
import Data.Function
import Data.Array.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import {-# SOURCE #-} Ordering
import {-# SOURCE #-} UnitTypes
import Data.Monoid
import Control.Concurrent.STM.TVar

-----------------
----- stats -----
-----------------

data HitPoints        =  HP Int     
data ManaPoints       =  MP Int
data ManaConsumption  =  MC Int
data DamageValue      =  Dmg Int
data Cooldown         =  CD Integer
                              
class Stat s where
    statValue :: s -> Int

    statWrap :: Int -> s

    statModify :: (Int -> Int) -> (s -> s)
    statModify f  =  statWrap . f . statValue

    statVal :: (Functor f) => (Int -> f Int) -> s -> f s
    statVal  =  lens statValue (const statWrap)

    statName :: s -> String

    statColor :: s -> Color
    statColor _  =  White

    statColorExt :: Stats -> s -> Color
    statColorExt _  =  statColor

    statColorIntensity :: s -> ColorIntensity
    statColorIntensity _  =  Vivid

    statColorIntensityExt :: Stats -> s -> ColorIntensity
    statColorIntensityExt _  =  statColorIntensity

    statMaxDigits :: s -> Int

    printStatExt :: Stats -> s -> IO ()
    printStatExt stats s  =  do
        setSGR [SetColor Foreground (statColorIntensityExt stats s) (statColorExt stats s)]
        putStr $ statName s ++ ": " ++ extend (statMaxDigits s) ' ' (show $ statValue s)
        setSGR []


data Stats

-----------------
----- skill -----
-----------------

data Skill 

data CastResult  =  CastSuccess 
                 |  CastInterrupted
                 |  CastFailTargetNA 

data SkillInfluence 


----------------
----- buff -----
----------------

-- buff template
data BuffT  

data Buff 

type ExpireCond  =  Target -> IO Bool

----------------
----- unit -----
----------------


data Unit  

data Side  =  LeftSide
           |  RightSide
       
type UnitId  =  Int

data Casting  

data UnitLog  =  UnitLog { _logText :: IO String
                         }


-----------------
----- field -----
-----------------

data Field  

type WorldChange  =  Field -> IO ()


----------------------------
----- target filtering -----
----------------------------

type Caster  =  Unit
type Target  =  Unit

type TargetSelection  =  Skill -> Caster -> Target -> All

data Relation  =  Self
               |  Ally
               |  Enemy
                    

-------------------------------
------ target preference ------
-------------------------------

data TargetPreferWay  =  PreferId UnitId
                      |  PreferLocked
                      |  PreferType UnitType OrderedPref
                      |  PreferNoGroup OrderedPref
                      |  NoNextTarget

data OrderedPref  =  OrderedPref CmpPref PrefCategory
                  |  RandomPref

type PrefCategory  =  Lens' Unit Int

data CmpPref  =  Lowest
              |  Highest

type LockedTargets  =  [UnitId]
                                  
type TargetPrefer  =  ReaderT LockedTargets (OrderT [Unit] IO) ()

------------
-- lenses --
------------

stats :: Lens' Unit Stats

unitSide :: Getter Unit Side
                 
