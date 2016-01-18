{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, Rank2Types #-}

module StatsLens where

import Control.Lens
import LensM
import {-# SOURCE #-} Unit.Variety
import Data
import Stats
import Data.Monoid
import Data.List
import Data.Function
import Unit.Type

class WithStats w where
    makeStat :: Stat s => Lens' Stats s -> Lens' w Int

    fakeNewUnit :: w -> w  -- not for export 

    hp :: Lens' w Int
    hp = makeStat $ __hp__

    mp :: Lens' w Int
    mp = makeStat $ __mp__

    maxStat :: Lens' w Int -> Getter w Int
    maxStat stat  = to fakeNewUnit.stat 
 
    revStat :: Functor f => Lens' w Int -> LensLike' f w Int
    revStat stat  =  lens get set
      where
        get u         =  u^.maxStat stat - u^.stat
        set u rem_hp  =  u & stat .~ u^.maxStat stat - rem_hp

boundStat :: Stat s => Lens' Stats s -> Lens' Unit Unit
boundStat stat  =  lens id $ const bound
  where
    stat' :: Functor f => LensLike' f Unit Int
    stat'  =  stats.stat.statVal

    bound :: Unit -> Unit
    bound u  =  u & stat'.bounding 0 (u^.maxStat stat') %~ id

bakeStat :: Functor f => LensLike' f FakeUnit Int -> LensLike' f Unit Int
bakeStat  =  fmap $ modifyAs.mod
  where
    mod :: Lens' FakeUnit FakeUnit
    mod  =  lens (\u -> u^.mod' u) (\u n -> u & mod' u .~ n)

    mod' :: FakeUnit -> Lens' FakeUnit FakeUnit
    mod'  =  appEndo . (^.statsF.modifiers.to sortMod.traversed.to _modifierEffect.to Endo)  

    sortMod  =  sortBy (compare `on` _modifierPriority)
  

    modifyAs :: Lens' Unit FakeUnit
    modifyAs  =  lens toFake setAsFake 
      where
        toFake :: Unit -> FakeUnit
        toFake Unit{ _unitType = t, _stats = s }  =  
            FakeUnit{ _unitTypeF = t, _statsF = s }
    
        setAsFake :: Unit -> FakeUnit -> Unit
        setAsFake u FakeUnit{ _unitTypeF = t, _statsF = s }  =  
            u { _unitType = t, _stats = s }


instance WithStats Unit where 
    makeStat stat  =  boundStat stat . bakeStat (makeStat stat)  

    fakeNewUnit Unit{ _unitType = t }  =  Unit 
        { _unitType = t
        , _stats    = initStats t
        , _side     = undefined
        , _casting  = undefined
        , _unitId   = undefined
        , _unitLog  = undefined
        }



instance WithStats FakeUnit where
    makeStat stat  =  statsF.stat.statVal

    fakeNewUnit u  =  u & statsF .~ (initStats $ _unitTypeF u)


   
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


