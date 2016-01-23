{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Viz.Field
    ( battleV,
      FieldEvent(FinishField, SetMessage)

    ) 
where

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
import Data.Default
import Graphics.Vty.Input.Events
import Graphics.Vty.Attributes
import Brick.Widgets.Center
import Data.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Chan
import Graphics.Vty.Input.Events
import Graphics.Vty
import Viz.Common
import Data.IORef
import Control.Monad.Fix
import Control.Concurrent

data FieldEvent  =  FEvent Event
                 |  UpdateField
                 |  FinishField
                 |  SetMessage (Maybe String)

data FieldState  =  FS 
    { _fieldL   :: Field
    , _snapshot :: Widget
    , _message  :: Maybe String
    }

makeLenses ''FieldState

battleV :: Field -> Chan FieldEvent -> IO () -> IO ()
battleV field chan finish  =  do
    stopped <- newIORef False
    -- displaying
    forkIO $ fix $ \cont -> do
        writeChan chan UpdateField
        threadDelay $ 2 * 10^4
        stop <- readIORef stopped
        unless stop cont
        
    void $ liftIO $ customMain (mkVty def) chan app $ FS 
        { _fieldL = field
        , _snapshot = emptyWidget
        , _message  = Nothing
        }

    writeIORef stopped True
    finish

app :: App FieldState FieldEvent
app  =  App 
    { appDraw = pure . gameLimit . (^.snapshot)
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent 
    , appStartEvent = return
    , appAttrMap = const def 
    , appLiftVtyEvent = FEvent
    }
  where
    handleEvent s (FEvent (EvKey KEsc _))  
                                        =  halt s
    handleEvent s (FEvent (EvKey _ _))  =  continue s
    handleEvent s (FEvent _)            =  continue s
    handleEvent s FinishField           =  halt s
    handleEvent s UpdateField           =  liftIO (takeSnapshot s) >>= continue
    handleEvent s (SetMessage m)        =  continue $ s & message .~ m

takeSnapshot :: FieldState -> IO FieldState
takeSnapshot s  =  (s & ) . (snapshot .~ ) <$> renderField s

renderField :: FieldState -> IO Widget
renderField s  =  do
    let field = s^.fieldL
    units <- fieldUnits field
    let sides = partition (( == LeftSide) . _side) units

    [leftWidget, rightWidget] <- mapM renderSide $ sides^.both.to pure
    let messageWidget = hLimit 30 $ center . str . fromMaybe " " $ s^.message
    let result = hBox [leftWidget, messageWidget, rightWidget]
    return result 
  where
    renderSide :: [Unit] -> IO Widget
    renderSide units = do
       uWidgets <- forM units $ fmap (padBottom $ Pad 1) . fmap hCenter . renderUnit 
       return $ vBox $ uWidgets 

renderUnit :: Unit -> IO Widget
renderUnit u  =  do
    let cast  =  u^.casting
    castProgress <- (realToFrac . fromMaybe 0) <$> sequence (cast^?_Cast.progress) 
    let label = str $ "#" ++ show (u^.unitId)
    
        renderName = str $ (alignLeft 15 . show) $ u^.unitType
        renderHp = padRight Max $ renderStat hpAttrs "HP" statWidth hp
        renderMp = renderStat mpAttrs "MP" statWidth mp
        renderStatDelay = str $ flip replicate ' ' $  statDelayWidth
        renderStats = hBox $ [renderHp, renderMp]
        renderCastProgress = updateAttrMap (applyAttrMappings progressAttrs) $ 
            progressBar (skillName . _castSkill <$> cast^?_Cast) castProgress 
        
        row1 = renderName <+> str "   " <+> renderCastProgress
        row2 = renderStats 

        body = row1 <=> row2
    
    return $ updateAttrMap (applyAttrMappings unitAttrs) 
           $ hLimit outwardWidth 
           $ borderWithLabel label body
  where
    outwardWidth = 50
    width = outwardWidth - 2
    statDelayWidth = 3
    statWidth = (width - statDelayWidth) `div` 2
    unitAttrs  =   
        [ (progressCompleteAttr,   bg green)
        , (progressIncompleteAttr, bg yellow)
        ]
    progressAttrs  = 
        [ (mempty, fg $ rgbColor 70 0 100)
        , (progressCompleteAttr, bg $ brightYellow)
        , (progressIncompleteAttr, bg $ 
            if hasn't (casting._Stunned) u then yellow else brightRed
          )                                    
        ]
    hpAttrs  = 
        [ (mempty, if (u^.hp) > 0
            then fg $ rgbColor 200 100 100 
            else fg $ rgbColor 100  20  20  
          )
        , (progressCompleteAttr, bg $ rgbColor 150 0 0)
        , (progressIncompleteAttr, bg $ rgbColor 30 0 0)
        ]
    mpAttrs  = 
        [ (mempty, fg $ rgbColor 100 100 200)
        , (progressCompleteAttr, bg $ rgbColor 0 0 150)
        , (progressIncompleteAttr, bg $ rgbColor 0 0 30)
        ]
    renderStat :: [(AttrName, Attr)] -> String -> Int -> Lens' Unit Int -> Widget
    renderStat attrs name size len  = 
        let val = show (u^.len) ++ " / " ++ show (u^.maxStat len)
            txt = name ++ alignRight (size - length name) val
            part = fromRational $ fromMaybe 0 $ u^.partStat len
            res = hLimit size $ progressBar (Just txt) $ part
        in  updateAttrMap (applyAttrMappings attrs) res


