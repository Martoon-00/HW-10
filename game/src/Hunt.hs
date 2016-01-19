module Hunt where

import System.Console.ANSI
import Control.Monad
import Control.Concurrent
import Data.Time.Units
import Stats
import Unit.Variety
import Unit.Common
import Display
import Control.Lens
import Control.Monad.State
import Control.Monad
import Text.Read.HT
import Data.Maybe
import Field
import Data
import Control.Lens.Traversal
import Data.Array.IO
import Control.Concurrent.Async
import Data.List
import Data.Monoid
import Data.Char
import Unit.Type
import Unit.Attack
import StatsLens
import Control.Arrow
import Control.Parallel.Strategies
import FieldAccess
import Graphics.Vty.Input.Events
import Graphics.Vty
import Brick
import Viz.Field
import Data.Default

battle :: IO ()
battle  =  do
    setTitle "Battle field"
    prepare >>= fillField >>= startBattle
    
startBattle :: Field -> IO ()
startBattle field  =  do
    allUnits <- fieldUnits field
    fights <- forM allUnits $ \unit -> do
        let startedFight = fight field unit
        async $ evalStateT startedFight []

    stopDispListener <- newEmptyMVar
    displaying <- async $ displayField field $ isJust <$> tryTakeMVar stopDispListener -- take care! mutable arrays are in danger!
    winner <- waitForFightEnd field $ mapM_ cancel fights
              
    putMVar stopDispListener ()
    -- wait displaying

    putStrLn $ (\w -> "Finish! " ++ w ++ " team wins!") 
        $ case winner of
            LeftSide   -> "Left"
            RightSide  -> "Right"
    threadDelay $ 10^6

waitForFightEnd :: Field -> IO () -> IO Side
waitForFightEnd field onEnd  =  do
    threadDelay 100000
    armies <- partition (( == LeftSide)  . _side) <$> fieldUnits field
    -- assuming that only one team can win 
    let courpses = armies & both.traversed %~ identifyCourpse 
    let loosers  = courpses & both %~ (fmap head . sequence)   
    let defeated = getAlt $ loosers^.both.to Alt   
    if isJust defeated
        then onEnd >> return (oppositeSide $ fromJust defeated)
        else waitForFightEnd field onEnd

  where
    identifyCourpse unit = if unit^.dead
        then Just $ unit^.side
        else Nothing

screenSize :: Int
screenSize = 1000

displayField :: Field -> IO Bool -> IO ()
displayField field stopCheck  =  do
    chan <- newChan
    forkIO $ void $ customMain (mkVty def) chan (fieldApp field) emptyWidget
    fix $ \f -> do
        writeChan chan UpdateField
        threadDelay 10000
        stop <- stopCheck 
        if stop
            then writeChan chan FinishField
            else f

prepare :: IO ([UnitType], [UnitType])
prepare  =  do
    initCache <- defineCash
    putStrLn "Available unit types:"
    sequence_ $ map putStrLn $ zipWith (\i u -> replicate 8 ' ' ++ show i ++ ". " ++ show u ++ " [$" ++ show (unitCost u) ++ "]") [0 ..] availableUnits
    gatherUnits initCache

type Cache  =  Int
    
defineCash :: IO Cache
defineCash  =  do
    putStrLn "Define initial cache: "
    row <- getLine
    if all isSpace row
        then return 1000
    else do
        let cash = maybeRead row :: Maybe Cache
        if isJust cash
            then do
                let cash_ = fromJust cash
                if cash_ >= minCash
                    then return $ cash_
                    else do
                        putStrLn $ "At least $" ++ show minCash ++ " please"
                        defineCash
        else do
             putStrLn "Invalid number"
             defineCash
  where
    minCash = 50


gatherUnits :: Cache -> IO ([UnitType], [UnitType])
gatherUnits cache  =  do
    res <- execStateT (gatherUnitsFor LeftSide) (([], cache), ([], cache))
    return $ res & both %~ fst

gatherUnitsFor :: Side -> StateT (([UnitType], Cache), ([UnitType], Cache)) IO ()
gatherUnitsFor side  =  do
    cur <- get
    let cash = cur ^. branch . _2

    liftIO $ putStrLn $ show (fromEnum side + 1) ++ "-st player choice: ($" ++ show cash ++ " remaining)"
    line <- liftIO $ getLine
    if any ( == line) $ ["stop", "done", "ok"]
        then do
            cur <- get 
            when (getAny $ cur ^. both . _1 . to null . to Any) $ do
                liftIO $ putStrLn "Armies should not be empty" 
                again                                            
    else if any ( == line) $ ["no", "none", "skip", "-"]
        then continue
    else
        let no = (maybeRead line :: Maybe Int) in
        if isJust no && fromJust no >= 0 && fromJust no < length availableUnits
            then do      
                let t = availableUnits !! (fromJust no)
                let cost = unitCost t
                if cost <= cash
                    then do
                        put $ cur & branch %~ bimap ( ++ [t]) (flip (-) cost)
                        liftIO $ return ()
                        continue
                    else do
                        liftIO $ putStrLn "Not enough cache"
                        again
            else do
                liftIO $ putStrLn "Incorrect unit id"
                again
  where 
    branch :: (Functor f, Field1 s t a b, Field2 s t a b) => (a -> f b) -> s -> f t
    branch = case side of
        LeftSide  -> _1
        RightSide -> _2
    
    again = gatherUnitsFor side
    continue = gatherUnitsFor $ oppositeSide side


