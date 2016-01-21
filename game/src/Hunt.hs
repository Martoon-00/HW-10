module Hunt where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Lens.Traversal
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Array.IO
import Data.Char
import Data.Default
import Data.List
import Data.Monoid
import Data.Maybe
import Text.Printf

import Brick
import Data.Time.Units
import Graphics.Vty
import Graphics.Vty.Input.Events
import System.Console.ANSI
import Text.Read.HT

import Data
import Field
import FieldAccess
import Stats
import StatsLens
import Targeting.Preference
import Viz.DefineCache
import Viz.Field
import Viz.Gather
import Unit.Attack
import Unit.Common
import Unit.Type
import Unit.Variety

battle :: IO ()
battle  =  do
    setTitle "Battle field"
--    void $ runMaybeT $ 
--        prepare >>= fmap lift fillField >>= fmap lift startBattle

epicTest :: IO ()
epicTest  =  (=<<) startBattle $ fillField $ ([Warrior, Archer], [TrainingTarget]) & both.traversed %~ defTemplate
    -- void $ runMaybeT $ gatherUnitsV 10000
    -- runMaybeT (defineCacheV 100) >>= print

    
startBattle :: Field -> IO ()
startBattle field  =  do
    (sendEvent, finished) <- startDisplaying field

    forkIO $ do
        sequence $ ( >> threadDelay (10 * 10^5)) . sendEvent . 
            SetMessage . Just . printf "Battle will start in %d..." <$> 
            ([3, 2..1] :: [Int])
        sendEvent $ SetMessage $ Nothing
    
        allUnits <- fieldUnits field
        fights <- (>>=) (fieldUnits field) $ mapM $ 
            async . flip evalStateT [] . fight field
    
        winner <- flip evalStateT (0, 0) 
                $ runMaybeT
                $ waitForFightEnd field 
        mapM_ cancel fights
        (>>=) (fieldUnits field) $ mapM $ stopFight field . _unitId
                  
        sendEvent $ SetMessage $ Just 
            $ printf "Finish! %s team wins!" 
            $ case winner of
                Nothing        -> "No"
                Just LeftSide  -> "Left"
                Just RightSide -> "Right"
        threadDelay $ 3 * 10^6
        sendEvent $ FinishField
    takeMVar finished
    -- if interrupted with Esc, battle threads may continue in background

type TicksPassed  =  Int
type SumHealth    =  Int

waitForFightEnd :: Field -> MaybeT (StateT (SumHealth, TicksPassed) IO) Side
waitForFightEnd field  =  do
    liftIO $ threadDelay checkDelay
    allUnits <- liftIO $ fieldUnits field
    let armies = partition (( == LeftSide)  . _side) $ allUnits
    -- count total units health, if remain unchanged for too long then finish battle 
    let totalHp = sumOf (folded.hp) allUnits
        modifyHpCounter s = fromMaybe (totalHp, 0) $ do
            let (totalHp, tics) = s
            return (totalHp, tics + 1)     
    lift $ get >>= put. modifyHpCounter
    lift get >>= guard . ( < secondsTillDraw * (10^6) `div` checkDelay) . snd
    -- assuming that only one team can win 
    let courpses = armies & both.traversed %~ identifyCourpse 
        loosers  = courpses & both %~ (fmap head . sequence)   
        defeated = getAlt $ loosers^.both.to Alt   
    if isJust defeated
        then liftIO $ return (oppositeSide $ fromJust defeated)
        else waitForFightEnd field 
  where
    identifyCourpse unit = if unit^.dead
        then Just $ unit^.side
        else Nothing

    checkDelay = 100000

    secondsTillDraw = 20


type SendEvent  =  FieldEvent -> IO ()

startDisplaying :: Field -> IO (SendEvent, MVar ())
startDisplaying field  =  do
    evChan <- newChan
    finished <- newEmptyMVar
    forkIO $ battleV field evChan $ putMVar finished ()
    return $ (writeChan evChan, finished)


