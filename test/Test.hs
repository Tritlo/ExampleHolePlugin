{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:Data.List -fdefer-type-errors #-}
module Test where

import Data.List (head, last)

prop_length :: ([Int] -> Int) -> Bool
prop_length f = f [] == 0 && f [1] == 1

t :: [Int] -> Int
t = _satisfies_prop_length

main :: IO ()
main = print $ t [1,2,3]
