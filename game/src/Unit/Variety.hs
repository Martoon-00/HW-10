{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Unit.Variety where

import Stats
import Control.Lens
import Data
import Control.Concurrent.MVar
import Data.Monoid
import Relation
import Unit.Type
import FieldAccess
import Control.Concurrent
import Control.Monad
import Preference
import StatsLens
import Buff
import LensM
import Modifiers
import Functions


initStats :: UnitType -> Stats                   
initStats TrainingTarget  =  Stats (HP  100) (MP    0) []
initStats Warrior         =  Stats (HP  400) (MP    0) [] 
initStats Archer          =  Stats (HP  200) (MP    0) []
initStats Mage            =  Stats (HP  120) (MP  500) []
initStats Recharger       =  Stats (HP  120) (MP 1000) []
initStats Guardian        =  Stats (HP  100) (MP    0) [permanent $ incomingDamageMult 0.4]
initStats Protector       =  Stats (HP   70) (MP  100) [permanent $ manaShield 5]
initStats Catapult        =  Stats (HP  700) (MP    0) []
initStats Witch           =  Stats (HP   80) (MP  650) []
initStats Healer          =  Stats (HP  180) (MP 1200) []
initStats Rogue           =  Stats (HP  180) (MP    0) [] 
initStats Reviver         =  Stats (HP  320) (MP 3000) []


unitSkills :: UnitType -> [Skill]              
unitSkills TrainingTarget  =  []
unitSkills Warrior  =  
    [ simpleSkill hp NegativeInfluence 1 (Dmg 10) (CD 10) (MC  0)
    ]
unitSkills Archer  =  
    [ simpleSkill hp NegativeInfluence 1 (Dmg  4) (CD  3) (MC  0)
    ]
unitSkills Mage  =  
    [ simpleSkill hp NegativeInfluence 3 (Dmg 20) (CD 25) (MC 80)
    , Skill 
        { skillAction  =  onTarget $ over mp ( + 200)
        , skillSideEffect  =  noAction
        , skillInfluence  =  PositiveInfluence
        , skillTargetSelection  =  selectWithRelation Self
        , skillMultitarget  =  1        
        , _cd  =  CD 70
        , _mc  =  MC 0
        }
    ]
unitSkills Recharger  =  
    [ Skill 
        { skillAction  =  onTarget $ over mp ( + 5) 
        , skillSideEffect  =  noAction
        , skillInfluence  =  PositiveInfluence
        , skillTargetSelection  =  
            selectWithRelation Ally
         <> selectNotDead
         <> selectWithAboveZero (revStat mp) 
        , skillMultitarget  =  multiTargetInf
        , _cd  =  CD 3
        , _mc  =  MC 13
        }
    , Skill 
        { skillAction  =  noAction
        , skillSideEffect  =  applyEffect (CD 4) neverExpires $ onTarget $ over mp ( + 1)
        , skillInfluence  =  PositiveInfluence
        , skillTargetSelection  =  selectWithRelation Self
        , skillMultitarget  =  1        
        , _cd  =  CD 15
        , _mc  =  MC 0
        }
    ]
unitSkills Guardian  =  
    [ simpleSkill hp NegativeInfluence 1 (Dmg 25) (CD 40) (MC 0)
    ]
unitSkills Protector  =  
    [ simpleSkill hp NegativeInfluence 1 (Dmg  15) (CD 30) (MC 0)
    ]
unitSkills Catapult  =  
    [ simpleSkill hp NegativeInfluence 1 (Dmg 500) (CD 200) (MC 0)
    ]
unitSkills Witch  =  
    [ Skill 
        { skillAction  =  onTarget $ over (revStat hp) ( + 1)  
        , skillSideEffect  =  noAction
        , skillInfluence  =  NegativeInfluence
        , skillTargetSelection  =  
            selectBasingOnRelation
         <> selectNotDead
        , skillMultitarget  =  1
        , _cd  =  CD 10
        , _mc  =  MC 30
        }
    ]
unitSkills Healer  =  
    [ Skill 
        { skillAction  =  onTarget $ over hp ( + 3) 
        , skillSideEffect  =  noAction
        , skillInfluence  =  PositiveInfluence
        , skillTargetSelection  =  
            selectBasingOnRelation 
         <> selectNotDead
         <> selectWithAboveZero (revStat hp) 
        , skillMultitarget  =  1
        , _cd  =  CD 1
        , _mc  =  MC 3
        }
    ]
unitSkills Rogue  =  
    [ Skill 
        { skillAction  =  noAction
        , skillSideEffect  =  applyEffect (CD 30) neverExpires $ onTarget $ over (revStat hp) ( + 1)
        , skillInfluence  =  NegativeInfluence
        , skillTargetSelection  =  
            selectBasingOnRelation 
         <> selectNotDead
        , skillMultitarget  =  1
        , _cd  =  CD 3
        , _mc  =  MC 0
        }   
    ]
unitSkills Reviver  =  
    [ Skill 
        { skillAction  =  onTarget (set hp 100)
        , skillSideEffect  =  applyBuff $ healthMult 3 <$> expiresIn (CD 50)
        , skillInfluence  =  PositiveInfluence
        , skillTargetSelection  =  
            selectBasingOnRelation 
         <> \_ _ -> All . (^.dead)
        , skillMultitarget  =  1
        , _cd  =  CD 40
        , _mc  =  MC 200
        }   
    ]


simpleSkill :: Lens' Unit Int -> SkillInfluence -> Int -> DamageValue -> Cooldown -> ManaConsumption -> Skill
simpleSkill stat influence targets dmg cd mc  =  Skill 
    { skillAction  =  onTarget $ over stat $ modifyStat influence $ statValue dmg
    , skillSideEffect  =  noAction
    , skillInfluence  =  influence
    , skillTargetSelection  =  
        selectBasingOnRelation
     <> selectNotDead
    , skillMultitarget  =  targets
    , _cd  =  cd
    , _mc  =  mc
    }
  where
    modifyStat NegativeInfluence  =  flip (-)
    modifyStat PositiveInfluence  =  (+)
    

-- Soul eater (on kill restores hp and increases mp, dmg scales with mp, mc = 0)

unitCost :: UnitType -> Int   
unitCost TrainingTarget  =  30 
unitCost Warrior         =  120
unitCost Archer          =  80
unitCost Mage            =  150
unitCost Recharger       =  90
unitCost Guardian        =  250
unitCost Protector       =  180
unitCost Catapult        =  500
unitCost Witch           =  200
unitCost Healer          =  220
unitCost Rogue           =  170
unitCost Reviver         =  300


multiTargetInf :: Int
multiTargetInf = 999

defTargetPrefer :: UnitType -> TargetPrefer
defTargetPrefer t  =  do
    targetPrefer t 
    (#) $ PreferNoGroup RandomPref
  where
    (#)  =  buildTargetPreferWay

    targetPrefer :: UnitType -> TargetPrefer    
    targetPrefer Rogue       =  return ()
    targetPrefer Healer      =  (#) $ PreferNoGroup $ OrderedPref Lowest hp
    targetPrefer _           =  (#) PreferLocked

availableUnits :: [UnitType]
availableUnits  =  [minBound .. maxBound]

onTarget :: AccessibleInto m => (Target -> Unit) -> Action m
onTarget f caster target  =  over (forUnit $ _unitId target) $ f  

noAction :: AccessibleInto m => Action m
noAction _ _ _  =  return ()

infixr 2 <+>
(<+>) :: AccessibleInto m => Action m -> Action m -> Action m
(h <+> g) c t f  =  (h c t f) >> (g c t f)

