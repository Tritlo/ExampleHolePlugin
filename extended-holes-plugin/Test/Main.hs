{-# OPTIONS -fplugin=ExtendedHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedTypedHoles #-}
module Main where
import ExtendedHolesPlugin
import Control.Monad
import Data.Dynamic
import Data.Typeable

f :: (a,b) -> a
f = _([Hoogle])

g :: (a,b) -> b
g = _$( exec $ do
        invoke Hoogle
        filterBy "Control.Monad"
        invoke Djinn)
main = return ()
