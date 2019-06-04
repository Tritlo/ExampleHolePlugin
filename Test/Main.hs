{-# OPTIONS -fplugin=HolePlugin
            -fplugin-opt=HolePlugin:600
            -funclutter-valid-hole-fits #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

f, g, h, i, j :: [Int] -> Int

f = _too_long
g = _only_Data_List
h = _only_Prelude
i = _sort_by_mod_desc
j = _

main :: IO ()
main = return ()
