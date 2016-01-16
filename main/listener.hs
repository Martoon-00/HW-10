module Listener where

import Control.Lens

data Listener a  =  Listener { satisfied :: a -> Bool
                             , action    :: IO ()
                             }

data Listeners a  =  Listerners [Listener]
