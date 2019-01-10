{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:hoogle -fforce-recomp #-}
-- Make sure to remove the hoogle opt if hoogle is not available locally
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

t :: [Int] -> Int
t = _module_Prelude

g :: [Int] -> Int
g = _module_Data_List

h :: [Int] -> Int
h = _satisfies_hole_info
 where hole_info :: String
       hole_info = "{'satisfies': ['prop_length', 'prop_3']}"

prop_length :: ([Int] -> Int) -> Bool
prop_length f = f [] == 0 && f [1,2,3] == 3


main :: IO ()
main = print $ t [1,2,3]
