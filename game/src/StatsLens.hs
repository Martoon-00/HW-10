{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Ratio

class WithStats w where
    makeIntStat :: Stat s Int => Lens' Stats s -> Lens' w Int

    makeBoolStat :: Stat s Bool => Lens' Stats s -> Lens' w Bool

    fakeNewUnit :: w -> w  -- not for export 

    hp :: Lens' w Int
    hp = makeIntStat $ __hp__

    mp :: Lens' w Int
    mp = makeIntStat $ __mp__

    visible :: Lens' w Bool 
    visible = makeBoolStat $ __visible__

    maxStat :: Lens' w Int -> Getter w Int
    maxStat stat  =  to fakeNewUnit.stat 
 
    revStat :: Functor f => Lens' w Int -> LensLike' f w Int
    revStat stat  =  lens get set
      where
        get u         =  u^.maxStat stat - u^.stat
        set u rem_hp  =  u & stat .~ u^.maxStat stat - rem_hp

    partStat :: Lens' w Int -> Getter w Rational
    partStat stat  =  to $ \u -> 
        let cur = u^.stat.to fromIntegral
            max = u^.maxStat stat.to fromIntegral
        in  if max == 0 then 0 else cur % max 
      

boundStat :: Stat s Int => Lens' Stats s -> Lens' Unit Unit
boundStat stat  =  lens id $ const $ bound  
  where
    stat' :: Functor f => LensLike' f Unit Int
    stat'  =  stats.stat.statVal

    bound :: Unit -> Unit
    bound u  =  u & stat'.bounding 0 (u^.maxStat stat') %~ id

bakeStat :: Functor f => LensLike' f FakeUnit a -> LensLike' f Unit a
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
            u{ _unitType = t, _stats = s }


instance WithStats Unit where 
    makeIntStat stat  =  boundStat stat . bakeStat (makeIntStat stat)  

    makeBoolStat stat  =  bakeStat (makeBoolStat stat)

    fakeNewUnit Unit{ _unitType = t }  =  Unit 
        { _unitType   = t
        , _stats      = initStats t
        , _side       = undefined
        , _casting    = undefined
        , _unitId     = undefined
        , _unitLog    = undefined
        , _unitPrefer = undefined
        }



instance WithStats FakeUnit where
    makeIntStat stat  =  statsF.stat.statVal
    
    makeBoolStat stat  =  statsF.stat.statVal

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


