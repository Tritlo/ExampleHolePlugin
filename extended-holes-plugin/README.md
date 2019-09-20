The Extended Holes Plugin
=================

The Extended Holes Plugin showcases how the new extended holes allows you
to communicate with plugins.
 
`DjinnBridge` is based on the `djinn-ghc` package by Alejandro Serrano but modified to use `TcM` directly.

Note! Needs a freshly built GHC from merge request [!1766](https://gitlab.haskell.org/ghc/ghc/merge_requests/1766) on GitLab.

Example Output
-----------------

Using this plugin, you can compile the following (using `cabal new-build test` with a freshly built GHC HEAD):

```haskell
{-# OPTIONS -fplugin=ExtendedHolesPlugin -funclutter-valid-hole-fits #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.Monad
import Language.Haskell.TH
import ExtendedHolesPlugin

f :: (a,b) -> b
f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0

main = return ()
```


And get the following output:

```
Main.hs:9:5: error:
    • Found hole: _$(...)0 :: (a, b) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 f :: forall a b. (a, b) -> b
               at Main.hs:8:1-15
      Or perhaps ‘_$(...)0’ is mis-spelled, or not in scope
    • In the expression: _$(...)0
      In an equation for ‘f’: f = _$(...)0
    • Relevant bindings include f :: (a, b) -> b (bound at Main.hs:9:1)
      Valid hole fits include
        (\ (_, a) -> a)
        (\ (_, a) -> seq (head (cycle (([]) ++ ([])))) a)
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        f :: (a, b) -> b
        snd :: forall a b. (a, b) -> b
  |
9 | f = _$(invoke "hoogle" & filterBy "Prelude" & invoke "djinn")0
  |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```
