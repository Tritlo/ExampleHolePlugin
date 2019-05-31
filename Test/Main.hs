{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:/home/tritlo/.hoogle/default-haskell-5.0.17
            -fno-show-type-app-of-hole-fits -fdefer-typed-holes #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

j :: [Int] -> Int
j = _only_Data_List


k :: [Int] -> Int
k = _only_Prelude


l :: [Int] -> Int
l = _invoke_hoogle

m :: [Int] -> Int
m = _invoke_djinn

main :: IO ()
main = print "hey"
