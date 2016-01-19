{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
import {-# SOURCE #-} Targeting.Ordering
import {-# SOURCE #-} Unit.Type
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
                              
class Stat s v | s -> v where


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
                 
