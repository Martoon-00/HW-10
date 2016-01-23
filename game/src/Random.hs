module Random (randomShuffle) 
where

import System.Random
import Data.Array.IO
import Data.Functor

randomShuffle :: [a] -> IO [a] 
randomShuffle l  =  newListArray (0, length l - 1) l
               >>=  randomShuffleArr 0  


randomShuffleArr :: Int -> IOArray Int a -> IO [a]
randomShuffleArr curPos arr  =  do
    n <- snd <$> getBounds arr
    if curPos > n then return []
    else do
        newPos <- randomRIO (curPos, n) :: IO Int
        v1 <- readArray arr curPos
        v2 <- readArray arr newPos
        writeArray arr newPos v1
        (:) v2 <$> randomShuffleArr (curPos + 1) arr
 
       
