{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Viz.DefineCache 
    ( defineCacheV
    ) 
where

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import Graphics.Vty.Input.Events
import Control.Monad
import Data.Default
import Control.Applicative
import LensM
import Control.Lens.Prism
import Control.Concurrent
import Graphics.Vty
import Data
import Data.Monoid
import Data.Maybe
import Text.Read.HT
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Data.Tuple
import Viz.Common

-- when user tryies to proceed with invalid cache, this temporaly sets to error message
type Nice  =  Maybe String

data CacheState  =  CS 
    { _editorr :: Editor
    , _nice    :: Nice
    , _escaped :: Bool
    }

makeLenses ''CacheState

cache :: Getter CacheState (Maybe Cache)
cache  =  editorr.to (maybeRead . head . getEditContents)

defineCacheV :: Cache -> MaybeT IO Cache
defineCacheV defCache  =  do
    chan <- liftIO newChan
    res <- liftIO $ customMain (mkVty def) chan app $ CS 
        { _editorr = ed
        , _nice    = Nothing
        , _escaped = False
        }
    
    guard $ res^.escaped
    
    let c = res^.cache
    if isJust c then MaybeT $ return c else error "Invalid cache" 
  where
    ed = editor "defineCache" (renderLine . head) (Just 1) (show defCache)
    
 
app :: App CacheState Event
app  =  App
    { appDraw = pure . gameLimit . draw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handle
    , appStartEvent = return
    , appAttrMap = const $ attrMap def []
    , appLiftVtyEvent = id
    }

draw :: CacheState -> Widget
draw s  =  
    let label      = str " Define initial cache "
        dollar     = fg green &> str "$"
        borderBody = dollar <+> renderEditor (_editorr s)
        border     = borderWithLabel label borderBody 
        warning    = vLimit 1 $ fg bad &> str $ fromMaybe " " (s^.nice)
        total      = bg cyan &> border <=> padTop (Pad 1) warning
        result     = center $ hLimit 30 total
    in result
  where
    bad :: Color
    bad  =  if isNothing $ s^.nice 
        then white
        else red
    
infixr 0 &>
(&>) :: Attr -> Widget -> Widget
(&>)  =  updateAttrMap . applyAttrMappings . pure . (mempty, )

renderLine :: String -> Widget
renderLine  =  str

handle :: CacheState -> Event -> EventM (Next CacheState)
handle s (EvKey KEnter _)  =  handleEnter s
handle s (EvKey KEsc _)  =  halt $ s & escaped .~ True
handle s e  =  if keysForEditor e 
    then (=<<) continue $ 
        return s & liftLensM editorr %~ (=<<) (handleEvent e) 
                 & liftLensM nice    .~ return Nothing
    else continue s

keysForEditor :: Event -> Bool
keysForEditor (EvKey (KChar c) _)
    |  c >= '0' && c <= '9'  =  True
    |  otherwise             =  False
keysForEditor (EvKey k _)  =  k `elem` 
    [ KDel
    , KBS
    , KUp
    , KDown
    , KLeft
    , KRight
    ]

handleEnter :: CacheState -> EventM (Next CacheState)
handleEnter s  =  do
    maybe (halt s) (continue . flip (set nice) s . Just) $ 
        maybe (Just "") checkNice $ s^.cache 

checkNice :: Cache -> Nice
checkNice c
    | c < minCache  =  Just $ "Set at least " ++ show minCache
    | otherwise     =  Nothing 
  where
    minCache = 50

