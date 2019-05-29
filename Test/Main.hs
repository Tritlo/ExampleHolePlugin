{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:out.fits -fno-show-type-app-of-hole-fits -fdefer-typed-holes #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

j :: [Int] -> Int
j = _with_prop_isLength

prop_isLength :: ([Int] -> Int) -> Bool
prop_isLength f = f [] == 0 && f [5,6] == 2

k :: [Int] -> Int
k = _with_prop_isHead

k3 = _a _b

prop_isHead :: ([Int] -> Int) -> [Int] -> Bool
prop_isHead f [] = True
prop_isHead f (x:xs) = (f (x:xs)) == x


main :: IO ()
main = print "hey"
