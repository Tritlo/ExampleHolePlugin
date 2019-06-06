{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:hoogle #-}
-- Make sure to remove the hoogle opt if hoogle is not available locally
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

t :: [Int] -> Int
t = _module_Prelude

g :: [Int] -> Int
g = _module_Data_List

main :: IO ()
main = print $ t [1,2,3]
