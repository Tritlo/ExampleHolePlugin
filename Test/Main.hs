{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:10
            -fno-show-type-app-of-hole-fits -fdefer-typed-holes #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

f :: [Int] -> Int
f = _only_Data_List


g :: [Int] -> Int
g = _only_Prelude


h :: [Int] -> Int
h = _default_sort

i :: [Int] -> Int
i = _sort_by_mod

j :: [Int] -> Int
j = _sort_by_mod_desc

main :: IO ()
main = print "hey"
