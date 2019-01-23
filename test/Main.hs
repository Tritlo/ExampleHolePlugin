{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:out.fits -fno-show-type-app-of-hole-fits -fdefer-typed-holes #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

j :: [Int] -> Int
j = _with_prop_isLength

prop_isLength :: ([Int] -> Int) -> Bool
prop_isLength f = f [] == 0 && f [5,6] == 2

main :: IO ()
main = print $ j [1,2,3]
