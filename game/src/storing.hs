module Storing where

class Scan s :: where
    scan s :: String -> IO s

class Store p where
    store p :: IO String

    