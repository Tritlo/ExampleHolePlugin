{-# OPTIONS -fplugin=DjinnHoogleModPlugin
            -funclutter-valid-hole-fits #-}
module Main where
import Control.Monad
f :: (a,b,c) -> a
f = _invoke_Djinn
f2 :: (a,b,c) -> a
f2 = _invoke_Hoogle
g :: [a] -> [[a]]
g = _invoke_Hoogle
h :: [[a]] -> [a]
h = _module_Control_Monad


main :: IO ()
main = return ()
