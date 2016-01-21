module Viz.Common where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

hGameSize, vGameSize :: Int
hGameSize = 170
vGameSize = 50

gameLimit :: Widget -> Widget
gameLimit = center 
          . borderWithLabel (str $ "  Game v0.9  ")
          . vLimit vGameSize 
          . hLimit hGameSize
