module Random (randomShuffle) 
where

import System.Random
import Data.Array.IO


randomShuffle :: [a] -> IO [a] 
randomShuffle l  =  do
    arr <- newListArray (0, length l - 1) l    
    randomShuffleArr 0 arr
    getElems $ arr

randomShuffleArr :: Int -> IOArray Int a -> IO ()
randomShuffleArr curPos arr  =  do
    n <- snd <$> getBounds arr
    if curPos >= n then return ()
    else do
        newPos <- randomRIO (curPos, n) :: IO Int
        v1 <- readArray arr curPos
        v2 <- readArray arr newPos
        writeArray arr curPos v2
        writeArray arr newPos v1
        randomShuffleArr (curPos + 1) arr
    