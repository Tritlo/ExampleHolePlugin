{-# OPTIONS -fplugin=HolePlugin -funclutter-valid-hole-fits #-}
module Main where

f :: a  -> a
f = _

g :: a -> b -> b
g = _

h :: Int -> Int
h = _

i :: (a,b) -> a
i = _

main :: IO ()
main = return ()
