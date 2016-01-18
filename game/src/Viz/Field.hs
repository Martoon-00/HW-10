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

renderField :: Field -> IO Widget
renderField field  =  do
    units <- fieldUnits field
    let sides = partition (( == LeftSide) . _side) units

    sideWidgets <- mapM renderSide $ sides^.both.to pure
    let [wl, wr] = hLimit 50 <$> sideWidgets

    return $ wl <+> wr 

  where
    renderSide :: [Unit] -> IO Widget
    renderSide units = do
        firstUnit <- renderUnit $ head units
        appendTailUnits <- forM (tail units) $ fmap (str "" <=> ) . renderUnit 
        let res = foldl (<=>) firstUnit appendTailUnits
        return res
        

renderUnit :: Unit -> IO Widget
renderUnit u  =  do
    let label = str $ "#" ++ show (u^.unitId)
    
        renderHp = str $ ("HP" ++ ) $ T.unpack $ T.justifyRight 4 ' ' $ T.pack $ show (u^.hp)
        renderMp = str $ ("MP" ++ ) $ T.unpack $ T.justifyRight 4 ' ' $ T.pack $ show (u^.mp)
        renderStats = renderHp <+> str " "  <+> renderMp
        row1 = str (u^.unitType.to show) <+> renderStats 
    
        row2 = progressBar Nothing 0.3 

        body = row1 <=> row2
    
    return $ borderWithLabel label $ vLimit 2 $ hLimit 40 body

data FieldEvent  =  FEvent Event
                 |  UpdateField

type FieldState  =  Widget


fieldApp :: Field -> App FieldState FieldEvent
fieldApp field  =  App 
    { appDraw = pure
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent field
    , appStartEvent = return
    , appAttrMap = const $ def 
    , appLiftVtyEvent = FEvent
    }
  where
    handleEvent f s (FEvent (EvKey _ _))  =  halt s
    handleEvent f s UpdateField         =  liftIO (renderField f) >>= continue





