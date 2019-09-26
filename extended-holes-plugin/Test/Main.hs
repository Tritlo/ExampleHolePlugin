{-# OPTIONS -fplugin=ExtendedHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedTypedHoles #-}
module Main where
import Control.Monad
import ExtendedHolesPlugin
import Control.Monad

import Language.Haskell.TH.Syntax


h :: ()
h = _$(liftData Hoogle)

main = return ()
