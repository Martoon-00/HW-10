module Functions where

import Data.List
import {-# SOURCE #-} Data
import Data.Time.Clock.POSIX
import Data.Time.Units

extendEnd :: Int -> a -> [a] -> [a]
extendEnd n def l  =  unfoldr next (n, l)
  where                                       
    next (k, []  )   
        | k <= 0     =  Nothing
        | otherwise  =  Just (def, (k - 1, []))
    next (k, x:xs)   =  Just (x  , (k - 1, xs))
     
extend :: Int -> a -> [a] -> [a]
extend n def l  =  foldr (:) l $ replicate (n - length l) def

infixr 8 .:
(.:) :: (c -> r) -> (a -> b -> c) -> a -> b -> r
(f .: g) x y  =  f $ g x y

infixr 8 ..:
(..:) :: (d -> r) -> (a -> b -> c -> d) -> a -> b -> c -> r
(f ..: g) x y z  =  f $ g x y z
                     
