module Unit.Attack where

import Control.Lens
import Control.Concurrent.MVar
import Control.Concurrent               
import Control.Monad
import Data
import Functions                                      
import Data.Time.Units
import Stats
import Control.Monad.State
import Targeting.Preference
import Unit.Variety
import LensM
import Data.Either as Either
import Data.Either.Combinators
import Data.Easy
import Data.List 
import Unit.Type          
import Data.Monoid
import FieldAccess
import Unit.Common
import Control.Monad.Trans.Maybe
import Data.Maybe
import StatsLens
import Data.Time.Units       
import Control.Concurrent.STM
import Targeting.Selection


fight :: Field -> Unit -> StateT LockedTargets IO ()
fight field caster'  =  do
  --  liftIO $ forkIO $ do 
  --      fieldBox & forUnit_' uid %~ updateModifiers
  --      threadDelay $ (`div` 2) $ fromIntegral $ toMicroseconds $ CD $ 1
    forever $ do
        -- choose skill & targets
        selectedSkill <- liftIO $ runMaybeT $ trySkills $ unitSkills unitType
        if isJust selectedSkill
            then initAttack $ fromJust selectedSkill
            else again    

  where
    trySkills :: [Skill] -> MaybeT IO Skill
    trySkills []     = fail ""
    trySkills (x:xs) = do
        ok <- lift $ trySkill x
        if ok 
            then return x
            else trySkills xs

    failWait :: IO ()
    failWait = do
        liftIO $ field & forUnit uid.casting .~ Nothing 
        threadDelay $ fromIntegral $ toMicroseconds $ CD $ 10   -- if cast failed, get stunned

    again :: StateT LockedTargets IO ()
    again = do
        liftIO $ failWait
        continue
        
    continue :: StateT LockedTargets IO ()
    continue = fight field caster'

    unitType :: UnitType
    unitType = _unitType caster'

    uid :: UnitId
    uid = _unitId $ caster'

    makeLog :: String -> IO UnitLog
    makeLog = createUnitLog (3 :: Second)

    log :: String -> IO ()
    log text = void $ forkIO $ do   
        logItem <- makeLog text
        field & forUnit uid.unitLog .~ logItem 

    canCast :: AccessibleInto m => Skill -> Field -> m Bool
    canCast skill field = do           
        caster <- unitById field uid
        return $ not $ caster^.mp < skill^.mc' || caster^.dead

    trySkill :: AccessibleInto m => Skill -> m Bool
    trySkill skill = do
        allUnits <- fieldUnits field                 
        let targetsAvailable = not $ null $ filter (selection' skill caster') $ allUnits  
        
        casterReady <- canCast skill field 
        return $ targetsAvailable && casterReady

    selection' :: Skill -> Caster -> Target -> Bool
    selection' skill = 
        let skillSelection = skillTargetSelection skill
            sel = runExtendedSelection skillSelection [minBound .. maxBound]
        in  getAll .: sel skill

    initAttack :: Skill -> StateT LockedTargets IO ()
    initAttack skill  =  do         
        -- init casting
        setTargets
        cast   <- liftIO $ newCasting skill 
        liftIO $ field & forUnit uid.casting .~ Just cast
        castBox <- liftIO newEmptyMVar 

        preTargets <- get
        liftIO $ log $ mappend "Casting at  " $ concat 
            $ map (\u -> "#" ++ show u ++ "  ") $ sort preTargets

        -- get CastSuccess when cast is completed
        liftIO $ void $ forkIO $ waitForSuccess $ putMVar castBox
        -- check whether it's alright, send any CastFail otherwise
        lockedTargets' <- get 
        liftIO $ void $ forkIO $ evalStateT (waitForFail $ putMVar castBox) lockedTargets'
                
        -- wait for cast result                              
        waitForCastResult skill $ readMVar castBox
      where          
        selection :: Caster -> Target -> Bool
        selection  =  selection' skill                          
         
        orderTargets :: LockedTargets -> [Unit] -> IO [Unit]
        orderTargets = buildTargetPrefer $ defTargetPrefer unitType 
        
        setTargets :: StateT LockedTargets IO ()
        setTargets = do
            lastLockedTargets <- get
            caster <- liftIO $ unitById field uid
            allUnits <- liftIO $ fieldUnits field
            
            targets <- liftIO 
                $ fmap (take $ skillMultitarget skill)
                $ orderTargets lastLockedTargets
                $ filter (selection caster)
                $ allUnits
            put $ targets^..traversed.unitId
                        
        recheckTargetsAvail :: AccessibleInto m => Field -> StateT LockedTargets m Bool
        recheckTargetsAvail field = do
            wasSelectedIds <- get            
            wasSelected <- lift $ forM wasSelectedIds $ unitById field

            caster <- lift $ unitById field uid
            let selected = filter (selection caster) wasSelected
            let selectedIds = selected^..traversed.unitId

            put selectedIds
            
            return $ not $ null selected

        waitForSuccess :: (CastResult -> IO ()) -> IO ()
        waitForSuccess signal = do
            threadDelay $ fromIntegral $ toMicroseconds $ skill^.cd
            signal CastSuccess 
                  
        waitForFail :: (CastResult -> IO ()) -> StateT LockedTargets IO ()
        waitForFail signal = do
            liftIO $ threadDelay $ 50000
            
            -- check state of caster and targets availability
            casterOk <- liftIO $ canCast skill field
            recheckTargetsAvail field

            recheckedTargets <- get     
                                                       
            maybe (waitForFail signal) (liftIO . signal) $ do
                boolToMaybe CastFailTargetNA $ null recheckedTargets
                boolToMaybe CastInterrupted  $ not casterOk

        waitForCastResult :: Skill -> IO CastResult -> StateT LockedTargets IO ()
        waitForCastResult skill castResult = do
            awaitedCasting <- liftIO $ castResult   -- blocks here
            case awaitedCasting of      
                CastInterrupted  -> liftIO $ log "Interrupted" >> failWait 
                CastFailTargetNA -> liftIO $ log "Targets N/A" >> failWait
                CastSuccess      -> tryAttack 
    
        tryAttack :: StateT LockedTargets IO ()
        tryAttack = do
            targetsId <- get
            newTargets <- liftIO $ do
                (res, newTargets) <- atomically $ flip runStateT targetsId $ do
                    canCast'   <- lift $ canCast skill field
                    targetsAv' <- recheckTargetsAvail field
                    either (return . ( >> failWait) . log) id $ do
                        boolToEither "Interrupted" () canCast' 
                        boolToEither "Targets N/A" () targetsAv' 
                        Right $ unitAct >> return (log "")
                res
                return newTargets
            put newTargets
            unitAct2
            

        unitAct :: StateT LockedTargets STM ()
        unitAct = do
            targetsId <- get 
            lift $ do
                caster <- unitById field uid
                let applySkill target = skillAction skill caster target field
            
                forM_ targetsId $ (=<<) applySkill . unitById field
                field & forUnit uid.mp -~ skill^.mc'

        unitAct2 :: StateT LockedTargets IO ()
        unitAct2 = do
            targetsId <- get 
            lift $ do
                caster <- unitById field uid
                let applySkill target = skillSideEffect skill caster target field
                forM_ targetsId $ (=<<) applySkill . unitById field




