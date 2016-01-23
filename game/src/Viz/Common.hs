module Viz.Common where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Graphics.Vty.Attributes
import qualified Data.Text as T

hGameSize, vGameSize :: Int
hGameSize = 170
vGameSize = 50

gameLimit :: Widget -> Widget
gameLimit = center 
          . borderWithLabel (str $ "  Game v0.9  ")
          . vLimit vGameSize 
          . hLimit hGameSize

infixr 0 &>
(&>) :: Attr -> Widget -> Widget
(&>)  =  updateAttrMap . setDefault

alignRight :: Int -> String -> String
alignRight size  =  T.unpack . T.justifyRight size ' ' . T.pack

alignLeft :: Int -> String -> String
alignLeft size  =  T.unpack . T.justifyLeft size ' ' . T.pack
