{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Targeting.Selection where

import Data
import Data.Function
import Stats
import Control.Lens
import {-# SOURCE #-} Unit.Variety
import Data.Monoid
import Functions
import StatsLens

niceGuy :: Relation -> Bool
niceGuy  =  ( /= Enemy)  

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
selectNotDead  =  selectWithAboveZero hp

selectWithAboveZero :: Lens' Unit Int -> TargetSelection
selectWithAboveZero stat _ _ target  =  All $ target^.stat > 0


runSelectionOption :: SelectionOption -> TargetSelection
runSelectionOption Alive   _ _ t  =  All $ t^.alive
runSelectionOption Visible _ _ t  =  All $ t^.visible

select :: (Skill -> Caster -> Target -> Bool) -> ExtendedSelection
select  =  fmap include $ fmap . fmap . fmap $ All

include :: TargetSelection -> ExtendedSelection
include  =  ( , []) . pure 

disable :: SelectionOption -> ExtendedSelection
disable  =  ([], ) . pure

runExtendedSelection :: ExtendedSelection -> [SelectionOption] -> TargetSelection
runExtendedSelection (selection, disabled) defs  = 
    let enabled  =  runSelectionOption <$> filter (not . ( `elem` disabled)) defs
    in  (++) selection enabled ^. traversed 






    

    



