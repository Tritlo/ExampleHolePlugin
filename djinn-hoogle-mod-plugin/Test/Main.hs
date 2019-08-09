{-# OPTIONS -fplugin=DjinnHoogleModPlugin
            -funclutter-valid-hole-fits #-}
module Main where
import Control.Monad
f :: (a,b) -> a
f = _invoke_Djinn
g :: [a] -> [[a]]
g = _invoke_Hoogle
h :: [[a]] -> [a]
h = _module_Control_Monad


main :: IO ()
main = return ()
