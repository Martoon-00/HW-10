{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Viz.Field where

import Brick
import Field
import Data
import FieldAccess
import Data.List
import Data.Monoid
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Brick.Widgets.Border
import Brick.Widgets.ProgressBar
import StatsLens
import qualified Data.Text as T
import Data.Default
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Brick.Widgets.Center
import Data.Maybe

renderField :: Field -> IO Widget
renderField field  =  do
    units <- fieldUnits field
    let sides = partition (( == LeftSide) . _side) units

    sideWidgets <- mapM renderSide $ sides^.both.to pure
    let [wl, wr] = sideWidgets

    return $ hBox sideWidgets 

  where
    renderSide :: [Unit] -> IO Widget
    renderSide units = do
       uWidgets <- forM units $ fmap (padBottom $ Pad 1) . fmap hCenter . renderUnit 
       return $ vBox $ uWidgets 

renderUnit :: Unit -> IO Widget
renderUnit u  =  do
    let cast =  u^.casting
    castProgress <- (realToFrac . fromMaybe 0) <$> traverse _progress cast
    let label = str $ "#" ++ show (u^.unitId)
    
        renderName = str $ (alignLeft 15 . show) $ u^.unitType
        renderHp = renderStat hpAttrs "HP" 11 hp
        renderMp = renderStat mpAttrs "MP" 11 mp
        renderStats = renderHp <+> str "  "  <+> renderMp
        row1 = renderName <+> str "   " <+> renderStats 
   
        row2 = progressBar (skillName . _castSkill <$> cast) castProgress 

        body = row1 <=> row2
    
    return $ updateAttrMap (applyAttrMappings unitAttrs) 
           $ hLimit 70 
           $ borderWithLabel label body
  where
    unitAttrs  =   
        [ (progressCompleteAttr,   bg green)
        , (progressIncompleteAttr, bg yellow)
        ]
    hpAttrs  = 
        [ (mempty, if (u^.hp) > 0
            then fg $ rgbColor 200 150 150 
            else fg $ rgbColor 100  20  20  
          )
        , (progressCompleteAttr, bg $ rgbColor 150 0 0)
        , (progressIncompleteAttr, bg $ rgbColor 50 0 0)
       ]
    mpAttrs  = 
        [ (mempty, fg $ rgbColor 100 100 255)
        , (progressCompleteAttr, bg $ rgbColor 0 0 150)
        , (progressIncompleteAttr, bg $ rgbColor 0 0 50)
       ]
    renderStat :: [(AttrName, Attr)] -> String -> Int -> Lens' Unit Int -> Widget
    renderStat attrs name size len  = 
        let val = show (u^.len) ++ " / " ++ show (u^.maxStat len)
            txt = (++) name $ (++) " " $ alignRight size val
            bar = progressBar (Just txt) $ u^.partStat len.to fromRational
            res = hLimit (length txt) $ bar
        in  updateAttrMap (applyAttrMappings attrs) res

alignRight :: Int -> String -> String
alignRight size  =  T.unpack . T.justifyRight size ' ' . T.pack

alignLeft :: Int -> String -> String
alignLeft size  =  T.unpack . T.justifyLeft size ' ' . T.pack

data FieldEvent  =  FEvent Event
                 |  UpdateField
                 |  FinishField

type FieldState  =  Widget


fieldApp :: Field -> App FieldState FieldEvent
fieldApp field  =  App 
    { appDraw = pure
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent field
    , appStartEvent = return
    , appAttrMap = const def 
    , appLiftVtyEvent = FEvent
    }
  where
    handleEvent f s (FEvent (EvKey _ _))  =  halt s
    handleEvent f s (FEvent _)            =  continue s
    handleEvent f s FinishField           =  halt s
    handleEvent f s UpdateField           =  liftIO (renderField f) >>= continue





