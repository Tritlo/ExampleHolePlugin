{-# OPTIONS -fplugin=ExtendedHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedTypedHoles #-}
module Main where
import ExtendedHolesPlugin
import Control.Monad

f :: (a,b) -> a
f = _([Hoogle])


g :: (a,b) -> b
g = _$( exec $ do
        invoke Hoogle
        filterBy "Control.Monad"
        invoke Djinn)0

h :: (a,b) -> b
h = _$$( execTyped $ do
         filterBy "Prelude"
         invoke Djinn)1


main = return ()
