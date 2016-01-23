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
               |  VapourCannon
               |  ManaDrainTotem
    deriving (Eq, Enum, Bounded)

instance Show UnitType where
    show TrainingTarget  =  "Training target"
    show Warrior         =  "Warrior"
    show Archer          =  "Archer"
    show Mage            =  "Mage"
    show Recharger       =  "Recharger"
    show Guardian        =  "Guardian"
    show Protector       =  "Protector"
    show Healer          =  "Healer"
    show Witch           =  "Witch"
    show Rogue           =  "Rogue"
    show Catapult        =  "Catapult"
    show Reviver         =  "Reviver"
    show VapourCannon    =  "Vapour cannon"
    show ManaDrainTotem  =  "Mana-drain totem"

