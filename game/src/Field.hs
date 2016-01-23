{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Field where

import Control.Lens
import Display
import System.Console.ANSI
import Control.Monad
import Data.Maybe
import Control.Monad.Trans
import Data.List
import Data.Function          
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data
import Unit.Common
import Data.Array.IO
import Unit.Type
import qualified Data.Array.IArray as A

fillField :: ([UnitTemplate], [UnitTemplate]) -> IO Field
fillField (l, r)  =  do                                
    let li = zip [1 ..] l
    let ri = zip [length li + 1 ..] r
    leftUnits  <- traverse (\(i, u) -> newDefUnit LeftSide  u i) li
    rightUnits <- traverse (\(i, u) -> newDefUnit RightSide u i) ri
    liftM (Field . wrap) $ mapM (atomically . newTVar) $ leftUnits ++ rightUnits
  where
    wrap l  =  A.array (1, length l) $ zip [1 ..] l
    
armiesOnField :: Field -> IO ([Unit], [Unit])
armiesOnField (Field unitsArray)  =  partition (( == LeftSide) . _side) 
    <$> mapM (atomically . readTVar) (A.elems unitsArray)

instance Display Field where
    display field  =  do
        (l, r) <- armiesOnField field
        let n       =  maximum $ map length [l, r] 
        let paired  =  zip (expand n l) (expand n r)                 
        
        forM_ paired $ \(l, r) -> do  
            forMaybe l $ displayWithOffset 10
            forMaybe r $ displayWithOffset 110
            forM_ [1 .. 3] $ const $ putStrLn ""

      where
        bar  =  ProgressBar Yellow 18   
        forMaybe m f  =  fromMaybe (return ()) $ f <$> m 
        expand k l 
            | k < n      =  error "extend feels sad"
            | otherwise  =  replicate h Nothing ++ map Just l ++ replicate r Nothing
          where 
            n  =  length l
            h  =  (k - n) `div` 2
            r  =  (k - n) - h

        displayWithOffset :: Int -> Unit -> IO ()
        displayWithOffset off unit = do
            -- unit
            setCursorColumn off
            display unit
            putStrLn ""
            setCursorColumn off 
            -- cast bar
            display =<< ProgressBarFixed bar <$> fromMaybe (return 0) (unit^?casting._Cast.progress)
            putStr $ "   "
            -- log
            setSGR [SetColor Foreground Vivid Black]
            putStr =<< unit^.unitLog.logText
            putStr $ replicate 30 ' '
            setSGR []
            cursorUpLine 1
    


        
instance Rollback Field where
    rollback field  =  do
        (l, r) <- armiesOnField field
        rollback $ ( * 3) $ maximum $ map length [l, r]
                                                     
    
