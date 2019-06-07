{-# OPTIONS -fplugin=DjinnPlugin -funclutter-valid-hole-fits #-}
module Main where

f :: a  -> a
f = _

g :: a -> b -> b
g = _

i :: (a,b) -> a
i = _

j :: a -> a -> a
j = _


main :: IO ()
main = return ()
