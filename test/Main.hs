{-# OPTIONS -fplugin=HolePlugin #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

t :: [Int] -> Int
t = _with_module_Prelude

g :: [Int] -> Int
g = _with_module_Data_List

h :: [Int] -> Int
h = _with_hoogle

main :: IO ()
main = print $ t [1,2,3]
