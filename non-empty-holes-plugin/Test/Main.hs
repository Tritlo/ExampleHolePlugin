{-# OPTIONS -fplugin=NonEmptyHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NonEmptyTypedHoles #-}
module Main where
import NonEmptyHolesPlugin
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

h :: (a,b) -> b
h = _$$( execTyped $ do
        invoke Hoogle
        filterBy "Control.Monad"
        invoke Djinn)


data A = A | B | C

b :: A
b = A

j :: ()
j = _([A,b])

main = return ()
