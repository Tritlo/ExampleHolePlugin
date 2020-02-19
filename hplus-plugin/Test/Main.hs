{-# OPTIONS -fplugin=HPlusPlugin -fplugin-opt=HPlusPlugin:/home/tritlo/hoogle_plus #-}
module Main where


t :: ((a -> b), a) -> b
t = _

main :: IO ()
main = return ()
