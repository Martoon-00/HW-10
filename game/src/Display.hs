{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

module Display where

import System.Console.ANSI

class Display d where
    display :: d -> IO ()


class Rollback d where
    rollback :: d -> IO ()

instance Rollback Int where
    rollback  =  cursorUpLine


data ProgressBar  =  ProgressBar Color Int
data ProgressBarFixed  =  ProgressBarFixed ProgressBar Double

instance Display ProgressBarFixed where
    display (ProgressBarFixed (ProgressBar col len) part)  =  do
        setSGR [SetColor Background Vivid col]
        putStr $ begin
        setSGR [SetColor Background Dull  col]
        putStr $ end
        setSGR []

      where
        edge = min len $ floor $ (fromIntegral (len + 1) * part)
        (begin, end) = splitAt edge $ replicate len ' '