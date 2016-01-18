module Unit.Type where

-- TODO: Perfect variant is place data declaration to data.hs, and constructors here
-- TODO: Try using ADT-syntax

data UnitType  =  TrainingTarget
               |  Warrior
               |  Archer
               |  Mage
               |  Recharger
               |  Guardian
               |  Protector
               |  Healer
               |  Witch
               |  Rogue
               |  Catapult
               |  Reviver
    deriving (Read, Show, Eq, Enum, Bounded)

