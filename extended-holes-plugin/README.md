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
{-# LANGUAGE ExtendedTypedHoles #-}
module Main where
import ExtendedHolesPlugin
import Control.Monad
import Language.Haskell.TH.Syntax (liftData)

f :: (a,b) -> a
f = _("invoke hoogle & filterBy Prelude & invoke djinn")

g :: (a,b) -> b
g = _$( exec $ do
        invoke Hoogle
        filterBy "Control.Monad"
        invoke Djinn)

main = return ()
```


And get the following output:

```

Main.hs:10:5: error:
    • Found hole: _(...) :: (a, b) -> a
      Where: ‘b’, ‘a’ are rigid type variables bound by
               the type signature for:
                 f :: forall a b. (a, b) -> a
               at Main.hs:9:1-15
      Or perhaps ‘_(...)’ is mis-spelled, or not in scope
    • In the expression: _(...)
      In an equation for ‘f’: f = _(...)
    • Relevant bindings include
        f :: (a, b) -> a (bound at Main.hs:10:1)
      Valid hole fits include
        (\ (a, _) -> a)
        (\ _ -> head (cycle (([]) ++ ([]))))
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        f :: (a, b) -> a
        fst :: forall a b. (a, b) -> a
   |
10 | f = _("invoke hoogle & filterBy Prelude & invoke djinn")
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Main.hs:13:5: error:
    • Found hole: _$(...) :: (a, b) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 g :: forall a b. (a, b) -> b
               at Main.hs:12:1-15
      Or perhaps ‘_$(...)’ is mis-spelled, or not in scope
    • In the expression: _$(...)
      In an equation for ‘g’: g = _$(...)
    • Relevant bindings include
        g :: (a, b) -> b (bound at Main.hs:13:1)
      Valid hole fits include
        (\ (_, a) -> a)
        Hoogle: Prelude fst :: (a, b) -> a
        Hoogle: Data.Tuple fst :: (a, b) -> a
        g :: (a, b) -> b
   |
13 | g = _$( exec $ do
   |     ^^^^^^^^^^^^^...
```
