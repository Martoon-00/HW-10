{-# LANGUAGE Rank2Types #-}

module Relation where

import Data
import Data.Function
import Stats
import Control.Lens
import {-# SOURCE #-} Unit.Variety
import Data.Monoid
import Functions
import StatsLens

niceGuy :: Relation -> Bool
niceGuy  =  ( /= Enemy)  -- thanks, cap

relationOf :: Unit -> Unit -> Relation
relationOf u1 u2 
    | ((==) `on` _unitId) u1 u2  =  Self
    | ((==) `on` _side  ) u1 u2  =  Ally
    | otherwise                  =  Enemy

selectBasingOnRelation :: TargetSelection
selectBasingOnRelation skill caster target  =  All $ decide attitude relation
  where
    relation  =  relationOf caster target

    attitude  =  skillInfluence skill
                                                  
    decide NegativeInfluence  =  not . niceGuy
    decide PositiveInfluence  =  niceGuy

selectWithRelation :: Relation -> TargetSelection
selectWithRelation rel _  =  All . ( == rel) .: relationOf 

selectNotDead :: TargetSelection
selectNotDead _ _  =  All . not . (^.dead)

selectWithAboveZero :: Lens' Unit Int -> TargetSelection
selectWithAboveZero stat _ _ target  =  All $ target^.stat > 0

