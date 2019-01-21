{-# OPTIONS -fplugin=HolePlugin #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

{-
t :: [Int] -> Int
t = _with_module_Prelude

g :: [Int] -> Int
g = _with_module_Data_List

h :: [Int] -> Int
h = _with_hoogle
-}

j :: [Int] -> Int
j = _with_prop_isLength

prop_isLength :: ([Int] -> Int) -> Bool
prop_isLength f = f [] == 0 && f [5,6] == 2

main :: IO ()
main = print $ j [1,2,3]
