{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Viz.Gather 
    ( gatherUnitsV
    )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.Dialog
import Control.Lens
import Graphics.Vty.Input.Events
import Control.Monad
import Data.Default
import Control.Applicative
import LensM
import Control.Lens.Prism
import Data hiding (units, _units)
import Control.Concurrent
import Graphics.Vty
import Data.Monoid
import Data.Maybe
import Text.Read.HT
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Unit.Type
import Viz.Common
import qualified Data.Vector as V
import Unit.Variety (availableUnits)
import Unit.Common
import Data.Tuple

type Nice  =  Maybe String

data Preproceed  =  Proceed
                 |  Back
    deriving (Enum, Eq, Bounded, Show)

data Gather  =  Gather
    { _forces     :: (Forces, Forces)
    , _current    :: Side
    , _escaped    :: Bool
    , _nice       :: Nice
    , _preproceed :: Maybe (Dialog Preproceed)
    , _avUnitList :: List UnitType
    }

data Forces  =  Forces
    { _cache :: Cache
    , _units :: [UnitTemplate]
    }

makeLenses ''Gather
makeLenses ''Forces

gatherUnitsV :: Cache -> MaybeT IO ([UnitTemplate], [UnitTemplate])
gatherUnitsV initCache  =  do
    chan <- liftIO newChan
    let initForces = Forces
            { _cache   = initCache
            , _units   = []
            }
    gathered <- liftIO $ customMain (mkVty def) chan app $ Gather
        { _escaped = False
        , _forces = (initForces, initForces)
        , _current = LeftSide
        , _nice = Nothing
        , _preproceed = Nothing
        , _avUnitList = list "avUnits" (V.fromList availableUnits) 1
        }  
    
    guard $ gathered^.escaped
    return $ gathered^.forces & both %~ (^.units)

app :: App Gather Event
app  =  App
    { appDraw = fmap gameLimit . draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handle . ( & nice .~ Nothing)
    , appStartEvent = return
    , appAttrMap = const $ attrMap def []
    , appLiftVtyEvent = id
    }

draw :: Gather -> [Widget]
draw g  =  catMaybes 
    [ drawPrequit g 
    , Just $ drawMain g
    ]

drawMain :: Gather -> Widget
drawMain g  =  
    let title = str " Choose units "
        forcesBox1 = borderWithLabel (str " Left side ") $ drawForcesList $ 
                     g^.forces._1.units
        forcesBox2 = borderWithLabel (str " Right side ") $ drawForcesList $ 
                     g^.forces._2.units
        avUnitBox  = hLimit 30 $ 
                     borderWithLabel (str " Select from ") $ drawAvUnitList $
                     g^.avUnitList
        result = hBox [forcesBox1, avUnitBox, forcesBox2]
    in result

drawPrequit :: Gather -> Maybe Widget
drawPrequit g  =  modify . flip renderDialog body <$> g^.preproceed
  where
    modify  =  center . border 
    body = padAll 1 $ center $ str "Are you ready to start battle?"


drawAvUnitList :: List UnitType -> Widget
drawAvUnitList  =  flip renderList renderLine
  where
    renderLine selected = str . ((++) $ if selected then "  " else "") . show

drawForcesList :: [UnitTemplate] -> Widget
drawForcesList  =  flip renderList renderLine . flip (list "lol") 2 . V.fromList
  where
    renderLine selected unit = 
        let row1 = str $ show $ unit^.unitTypeT
            row2 = str ""
        in  row1 <=> row2

handle :: Gather -> Event -> EventM (Next Gather)
handle s e@(EvKey c m)   
    -- preproceed
    | has _Just (s^.preproceed)      =  delegateToPreproceed s e              -- lens usage
    -- available units navigation 
    | c `elem` [ KUp, KDown, KLeft, KRight
               , KPageDown, KPageUp, KHome, KEnd]                                       
                                     =  delegateToAvUnitList s e
    -- proceed
    | c == KEnter                    =  proceed s
    -- choose unit
    | c == KChar ' '                 =  continue $ addCurrentUnit s
    -- quit
    | c == KChar 'q' && MCtrl `elem` m
                                     =  halt $ s & escaped .~ True
    | otherwise                      =  continue s

delegateToAvUnitList :: Gather -> Event -> EventM (Next Gather)
delegateToAvUnitList s e  =  (=<<) continue $
    return s & liftLensM avUnitList %~ (=<<) (handleEvent e) 

delegateToPreproceed :: Gather -> Event -> EventM (Next Gather)
delegateToPreproceed s e@(EvKey c _)
    | c == KEsc    =  close
    | c == KEnter && selected == Just Proceed 
                   = halt s 
    | c == KEnter && selected == Just Back 
                   =  close
    | c == KEnter  =  continue s
    | otherwise    =  navigate
  where
    close = continue $ s & preproceed .~ Nothing

    navigate = do
        let d = fromJust $ s^.preproceed
        d' <- handleEvent e d
        continue $ s & preproceed .~ Just d'
    
    selected = (preproceedButtons !! ) <$> 
        s^.preproceed.singular _Just.dialogSelectedIndexL


addCurrentUnit :: Gather -> Gather
addCurrentUnit gather  =  fromMaybe gather $ do
    let unitList = gather^.avUnitList
    selectionNo <- unitList^.listSelectedL 
    let newUnit = UnitTemplate
            { _unitTypeT   = (unitList^.listElementsL) V.! selectionNo  
            , _unitPreferT = return ()
            }
    return $ gather & onCurrentSide.units %~ ( |> newUnit)                -- lens usage
                    & current                %~ oppositeSide

onCurrentSide :: Lens' Gather Forces
onCurrentSide   =  lens (\g -> g^.forces.choose (g^.current)) 
                        (\g f -> g & forces.choose (g^.current) .~ f)
  where
    choose LeftSide   =  _1
    choose RightSide  =  _2

proceed :: Gather -> EventM (Next Gather)
proceed g
    | allOf (forces.both.units) (not . null) g                            -- lens usage
        = continue $ g & preproceed .~ Just confirmation
    | otherwise
        = continue $ g & nice .~ Just "At least one unit at each side"
  where
    buttons = swap . ( & id <<%~ show) <$> preproceedButtons              -- lens usage
    confirmation = dialog "preproceed" Nothing (Just (0, buttons)) 50

preproceedButtons :: [Preproceed]
preproceedButtons  =  [minBound .. maxBound]
