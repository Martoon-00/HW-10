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
import Functions
import Data.Function
import Data.Array.IO
import Data.Array
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Targeting.Ordering
import Unit.Type
import LensM
import Data.Monoid
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import qualified Data.Array.IArray 

-----------------
----- stats -----
-----------------

data HitPoints        =  HP Int     
    deriving (Show)
data ManaPoints       =  MP Int
    deriving (Show)
data ManaConsumption  =  MC Int
    deriving (Show)
data DamageValue      =  Dmg Int
    deriving (Show)
data Cooldown         =  CD Integer
    deriving (Show)
data Visibility       =  Vis Bool     
                         
class Show v => Stat s v | s -> v where
    statValue :: s -> v

    statWrap :: v -> s

    statVal :: (Functor f) => LensLike' f s v
    statVal  =  lens statValue (const statWrap)

    statName :: s -> String

    statColor :: s -> Color
    statColor _  =  White

    statColorExt :: Stats -> s -> Color
    statColorExt _  =  statColor

    statColorIntensity :: s -> ColorIntensity
    statColorIntensity _  =  Vivid

    statColorIntensityExt :: Stats -> s -> ColorIntensity
    statColorIntensityExt _  =  statColorIntensity

    statMaxDigits :: s -> Int

    printStatExt :: Stats -> s -> IO ()
    printStatExt stats s  =  do
        setSGR [SetColor Foreground (statColorIntensityExt stats s) (statColorExt stats s)]
        putStr $ statName s ++ ": " ++ extend (statMaxDigits s) ' ' (show $ statValue s)
        setSGR []


data Stats  =  Stats { _hp        :: HitPoints 
                     , _mp        :: ManaPoints
                     , _visible   :: Visibility
                     , _modifiers :: [Modifier]
                     }   
                 
data Modifier  =  Modifier 
    { _modifierPriority :: ModifierPriority
    , _modifierEffect   :: Lens' FakeUnit FakeUnit
    , _modifierExpired  :: ExpireCond 
    }

type ModifierPriority  =  Int
           

-----------------
----- skill -----
-----------------

data Skill  =  Skill { skillAction          :: Action STM
                     , skillSideEffect      :: Action IO 
                     , skillInfluence       :: SkillInfluence
                     , skillTargetSelection :: ExtendedSelection 
                     , skillMultitarget     :: Int
                     , skillName            :: String 
                     , _cd                  :: Cooldown
                     , _mc                  :: ManaConsumption
                     }
      
type Action m  =  Caster -> Target -> Field -> m ()

data CastResult  =  CastSuccess 
                 |  CastInterrupted
                 |  CastFailTargetNA

data SkillInfluence  =  NegativeInfluence
                     |  PositiveInfluence


----------------
----- buff -----
----------------

-- buff template
data BuffT  =  BuffT 
    { _buffAction    :: Target -> IO ()
    , _buffInfluence :: SkillInfluence
    , _expired       :: ExpireCond
    }

data Buff  =  Buff 
    { _buffTemplate :: BuffT
    , _buffTarget   :: Unit
    , _dispelled    :: Bool
    }

type ExpireCond  =  Target -> IO Bool


----------------
----- unit -----
----------------

data Unit  =  Unit { _unitType :: UnitType
                   , _stats    :: Stats
                   , _side     :: Side
                   , _casting  :: Maybe Casting
                   , _unitId   :: UnitId
                   , _unitLog  :: UnitLog
                   }

data Side  =  LeftSide
           |  RightSide
    deriving (Enum, Show, Eq, Ord)
       
type UnitId  =  Int
                    
data Casting  =  Casting { _castSkill :: Skill
                         , _progress  :: IO Double
                         }

data UnitLog  =  UnitLog { _logText :: IO String
                         }

data FakeUnit  =  FakeUnit { _unitTypeF :: UnitType
                           , _statsF    :: Stats
                           }

-----------------
----- field -----
-----------------

data Field  =  Field { units :: Array Int (TVar Unit)
                     }

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
    deriving (Eq, Ord, Enum, Show)
 
data SelectionOption
    =  Alive
    |  Visible
    deriving (Eq, Enum, Bounded, Show)

type ExtendedSelection  =  ([TargetSelection], [SelectionOption])

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

makeLenses ''Field
makeLenses ''Skill                 
makeLensesFor [ ("_hp", "__hp__")
              , ("_mp", "__mp__")
              , ("_visible", "__visible__")
              , ("_modifiers", "modifiers")
              ] ''Stats
makeLenses ''Unit                        
makeLenses ''FakeUnit                        
makeLenses ''Casting
makeLenses ''UnitLog
makeLenses ''Buff

unitSide :: Getter Unit Side
unitSide  =  to _side
                                   
