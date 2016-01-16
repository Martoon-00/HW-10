{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types #-}

module StatsLens where

import Control.Lens
import LensM
import {-# SOURCE #-} Units
import Data
import Stats
import Data.Monoid
import Data.List
import Data.Function
import UnitTypes

boundStat :: Stat s => Lens' Stats s -> Lens' Unit Unit
boundStat stat  =  lens id $ const bound
  where
    stat' :: Functor f => LensLike' f Unit Int
    stat'  =  stats.stat.statVal

    bound :: Unit -> Unit
    bound u  =  u & stat'.bounding 0 (u^.maxStat stat') %~ id

m_hp :: Functor f => LensLike' f Unit Int
m_hp  =  stats.__hp__.statVal

m_mp :: Functor f => LensLike' f Unit Int
m_mp  =  stats.__mp__.statVal

bakeStat :: Functor f => LensLike' f Unit Int -> LensLike' f Unit Int
bakeStat  =  (mod . )
  where
    mod :: Lens' Unit Unit
    mod  =  lens (\u -> u^.mod' u) (\u n -> u & mod' u .~ n)

    mod' :: Unit -> Lens' Unit Unit
    mod'  =  appEndo . (^.stats.modifiers.to sortMod.traversed.to _modifierEffect.to Endo)  

    sortMod  =  sortBy (compare `on` _modifierPriority)
  
hp :: Functor f => LensLike' f Unit Int
hp  =  boundStat __hp__ . bakeStat m_hp

mp :: Functor f => LensLike' f Unit Int
mp  =  boundStat __mp__ . bakeStat m_mp

hp' :: Getter Stats Int
hp'  =  __hp__.statVal

maxStat :: Lens' Unit Int -> Getter Unit Int
maxStat stat  =  to fakeNewUnit.stat


revStat :: Functor f => Lens' Unit Int -> LensLike' f Unit Int
revStat stat  =  lens get set
  where
    get u         =  u^.maxStat stat - u^.stat
    set u rem_hp  =  u & stat .~ u^.to fakeNewUnit.stat - rem_hp

fakeNewUnit :: Unit -> Unit
fakeNewUnit u  =  let t = _unitType u in 
    Unit
    { _unitType = t
    , _stats    = initStats t
    , _side     = undefined
    , _casting  = undefined
    , _unitId   = undefined
    , _unitLog  = undefined
    } 
    
-- dmg' :: Functor f => LensLike' f Skill Int
-- dmg'  =  dmg.statVal

cd' :: Functor f => LensLike' f Skill Int
cd'  =  cd.statVal

mc' :: Functor f => LensLike' f Skill Int
mc'  =  mc.statVal

dead :: Getter Unit Bool
dead  =  to $ ( <= 0) . ( ^.hp) 

alive :: Getter Unit Bool
alive  =  to $ ( > 0) . ( ^.hp) 

-- exhausted :: Getter Stats Bool
-- exhausted  =  to $ \stats -> statValue (_mp stats) < statValue (_mc stats)


