{-# OPTIONS -fplugin=DjinnPlugin -fplugin-opt=DjinnPlugin:2
            -funclutter-valid-hole-fits #-}
module Main where


f :: (a,b) -> b
f = _

g :: a -> b -> a
g = _


main :: IO ()
main = return ()
