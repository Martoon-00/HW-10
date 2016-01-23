{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

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
import Unit.Variety
import qualified Data.Easy as E
import Text.Printf

-- warning message
type Nice  =  Maybe String

-- is used when user tryied to quit, and confimation dialog appears
data Preproceed  =  Proceed
                 |  Back
    deriving (Enum, Eq, Bounded, Show)

-- available unit type
data AvUnit  =  Av UnitType
             |  Skip

data Gather  =  Gather
    { _forces     :: (Forces, Forces)
    , _current    :: Side
    , _escaped    :: Bool
    , _nice       :: Nice
    , _preproceed :: Maybe (Dialog Preproceed)
    , _avUnitList :: List AvUnit
    }

data Forces  =  Forces
    { _cache :: Cache
    , _units :: [UnitTemplate]
    }

makeLenses ''Gather
makeLenses ''Forces
makePrisms ''AvUnit

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
        , _avUnitList = avList
        }  
    
    guard $ not $ gathered^.escaped
    return $ gathered^.forces & both %~ (^.units)
  where
    avList = let l = Skip : (Av <$> availableUnits)
        in list "avUnits" (V.fromList l) 1


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
        forcesBox1 = forcesBox " Left side "  _1 LeftSide
        forcesBox2 = forcesBox " Right side " _2 RightSide
        avUnitBox  = hLimit 30 $ borderWithLabel (str " Select from ") 
                        $ drawAvUnitList ( <= g^.onCurrentSide.cache) 
                        $ g^.avUnitList
        main = hBox [forcesBox1, avUnitBox, forcesBox2]
        warn = hCenter $ str $ fromMaybe " " $ g^.nice
        result = main <=> warn  
    in result
  where
    curColor False = rgbColor 50 50 50
    curColor True  = white

    forcesBox :: String -> Lens' (Forces, Forces) Forces -> Side -> Widget
    forcesBox title branch side = 
        let selected = (g^.current == side)
        in  fg (curColor selected) &> borderWithLabel (str title) $
                drawForces (g^.forces.branch) selected

drawPrequit :: Gather -> Maybe Widget
drawPrequit g  =  modify . flip renderDialog body <$> g^.preproceed
  where
    modify  =  center . hLimit 90 . vLimit 6 
        . updateAttrMap (applyAttrMappings attrs)  
    body = center $ str "Are you ready to start the battle?"

    attrs = 
        [ (dialogAttr, white `on` cyan)
        , (buttonAttr, (rgbColor 160 90 140) `on` blue)
        , (buttonSelectedAttr, white `on` blue)
        ]

drawAvUnitList :: (Cache -> Bool) -> List AvUnit -> Widget
drawAvUnitList enoughCache  =  flip renderList renderLine
  where
    renderLine selected av = 
        let name = str $ ((++) $ if selected then "  " else "") $ showAv av
            money = avCost av
            money' = fg (enoughColor $ enoughCache money) &> str $ ('$' : ) $ 
                alignRight 3 $ show $ money
        in  money' <+> str "  " <+> padRight Max name
 
    showAv Skip  =  "[Skip]"
    showAv (Av t)  =  show t

    enoughColor True  = green
    enoughColor False = red

avCost :: AvUnit -> Cache
avCost Skip  =  0
avCost (Av t)  =  unitCost t

drawForces :: Forces -> Bool -> Widget
drawForces forces selected  =  vBox 
    [ str $ "Remaining cache: $" ++ show (forces^.cache)  
    , hBorder
    , Attr (SetTo underline) KeepCurrent KeepCurrent &> str "Chosen units"
    , drawForcesList $ forces^.units 
    ]

drawForcesList :: [UnitTemplate] -> Widget
drawForcesList  =  flip renderList renderLine . flip (list "lol") 2 . V.fromList
  where
    renderLine selected unit = 
        let row1 = str $ show $ unit^.unitTypeT
            row2 = str ""
        in  row1 <=> row2

handle :: Gather -> Event -> EventM (Next Gather)
handle s e@(EvKey c m)   
    -- quit
    | c == KChar 'q' && MCtrl `elem` m
                                     =  halt $ s & escaped .~ True
    -- preproceed
    | has _Just (s^.preproceed)      =  delegateToPreproceed s e              -- prism usage
    -- available units navigation 
    | c `elem` [ KUp, KDown, KLeft, KRight
               , KPageDown, KPageUp, KHome, KEnd]                                       
                                     =  delegateToAvUnitList s e
    -- proceed
    | c == KEnter                    =  proceed s
    -- choose unit
    | c == KChar ' '                 =  continue $ addCurrentUnit s
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

    navigate = (=<<) continue $ return s 
        & liftLensM (preproceed.singular _Just)                            -- prism usage
        %~ ( >>= handleEvent e)
 
    selected = (preproceedButtons !! ) <$> 
        s^.preproceed.singular _Just.dialogSelectedIndexL                  -- prism usage

addCurrentUnit :: Gather -> Gather
addCurrentUnit gather  =  either (flip (set nice) gather) id $ do
    let unitList = gather^.avUnitList
    selectionNo <- E.maybeToEither Nothing $ unitList^.listSelectedL 
    let selected = (unitList^.listElementsL) V.! selectionNo
        cost = avCost selected
    E.boolToEither (Just "Not enought cache") () $
        cost <= gather^.onCurrentSide.cache
    E.boolToEither (Just $ printf "No more than %d units" maxUnitNumber) () $
        lengthOf (onCurrentSide.units.folded) gather < maxUnitNumber
     || has _Skip selected    
    return $ gather & onCurrentSide.units %~ modForces selected  
                    & onCurrentSide.cache -~ avCost selected
                    & current             %~ oppositeSide
  where
    modForces :: AvUnit -> [UnitTemplate] -> [UnitTemplate]
    modForces Skip   = id
    modForces (Av t) = flip (|>) UnitTemplate                             -- lens usage
        { _unitTypeT   = t
        , _unitPreferT = defTargetPrefer t   
        } 
    

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

maxUnitNumber :: Int
maxUnitNumber  =  10
