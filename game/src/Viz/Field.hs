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
import qualified Data.Text as T
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
battleV field chan finished  =  do
    stopped <- newIORef False
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
    finished


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
    let messageWidget = center . str . fromMaybe " " $ s^.message
    let result = hBox [leftWidget, messageWidget, rightWidget]
    return result 
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
           $ hLimit 55 
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
        [ (mempty, fg $ rgbColor 200 150 150)
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
