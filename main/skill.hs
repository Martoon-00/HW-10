module Skill where

import Data

isCastSuccessful :: CastResult -> Bool
isCastSuccessful CastSuccess  =  True
isCastSuccessful _            =  False

