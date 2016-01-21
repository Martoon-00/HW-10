module Unit.Variety 
    ( initStats
    , unitSkills
    , unitCost
    , defTargetPrefer
    , availableUnits
    )
where
                           
import {-# SOURCE #-} Data
import {-# SOURCE #-} Unit.Type

initStats :: UnitType -> Stats

unitSkills :: UnitType -> [Skill]

unitCost :: UnitType -> Int

availableUnits :: [UnitType]

defTargetPrefer :: UnitType -> TargetPrefer

