{-# OPTIONS -fplugin=ExtendedHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad
import Language.Haskell.TH
import ExtendedHolesPlugin

f :: (a,b) -> b
f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0

main = return ()
