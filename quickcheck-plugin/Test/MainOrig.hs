{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:out.fits -fno-show-type-app-of-hole-fits -fdefer-typed-holes #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

j :: [Int] -> Int
j = _with_prop_behavesLikeLength

prop_behavesLikeLength :: ([Int] -> Int) -> Bool
prop_behavesLikeLength f = f [] == 0 && f [5,6] == 2

k :: [Int] -> Int
k = _with_prop_behavesLikeHead

prop_behavesLikeHead :: ([Int] -> Int) -> [Int] -> Bool
prop_behavesLikeHead f [] = True
prop_behavesLikeHead f (x:xs) = (f (x:xs)) == x


main :: IO ()
main = print "hey"
