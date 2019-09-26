{-# OPTIONS -fplugin=DjinnHoogleModPlugin -funclutter-valid-hole-fits #-}
module Main where
import Control.Monad

f :: (a,b) -> b
f = _invoke_Hoogle

main = return ()
