module Main where

import Hunt
import Unit.Type
import Field

main :: IO ()
main = battle

a :: IO ()
a  =  fillField ([Warrior, Archer, Recharger], [TrainingTarget]) >>= startBattle
