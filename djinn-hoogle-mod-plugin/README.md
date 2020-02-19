The Djinn Hoogle Module Plugin
=================

The Djinn Hoogle Module Plugin showcases some basic hole fit plugin functionality,
including how to combine multiple plugins into one by using the name of holes.

This plugin shows how `djinn` can be invoked by a hole fit plugin to synthesize simple programs, how
`hoogle` can be invoked by the compiler to search for a fit by the type, and how candidates can be
filtered by module.

`DjinnBridge` is based on the `djinn-ghc` package by Alejandro Serrano but modified to use `TcM` directly.

Note! Needs GHC 8.10

Example Output
-----------------

Using this plugin, you can compile the following (using `cabal new-build test` with a freshly built GHC HEAD):

```haskell
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
```


And get the following output:

```

Main.hs:6:5: error:
    • Found hole: _invoke_Djinn :: (a, b) -> a
      Where: ‘b’, ‘a’ are rigid type variables bound by
               the type signature for:
                 f :: forall a b. (a, b) -> a
               at Main.hs:5:1-15
      Or perhaps ‘_invoke_Djinn’ is mis-spelled, or not in scope
    • In the expression: _invoke_Djinn
      In an equation for ‘f’: f = _invoke_Djinn
    • Relevant bindings include f :: (a, b) -> a (bound at Main.hs:6:1)
      Valid hole fits include
        (\ (a, _) -> a)
        (\ _ -> head (cycle (h (g ([])) ++ h (g ([])))))
        f :: (a, b) -> a
        fst :: forall a b. (a, b) -> a
  |
6 | f = _invoke_Djinn
  |     ^^^^^^^^^^^^^

Main.hs:8:5: error:
    • Found hole: _invoke_Hoogle :: [a] -> [[a]]
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 g :: forall a. [a] -> [[a]]
               at Main.hs:7:1-17
      Or perhaps ‘_invoke_Hoogle’ is mis-spelled, or not in scope
    • In the expression: _invoke_Hoogle
      In an equation for ‘g’: g = _invoke_Hoogle
    • Relevant bindings include
        g :: [a] -> [[a]] (bound at Main.hs:8:1)
      Valid hole fits include
        Hoogle says: Data.List subsequences :: [a] -> [[a]]
        Hoogle says: Data.List permutations :: [a] -> [[a]]
        g :: [a] -> [[a]]
        repeat :: forall a. a -> [a]
        return :: forall (m :: * -> *) a. Monad m => a -> m a
        pure :: forall (f :: * -> *) a. Applicative f => a -> f a
        (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
  |
8 | g = _invoke_Hoogle
  |     ^^^^^^^^^^^^^^

Main.hs:10:5: error:
    • Found hole: _module_Control_Monad :: [[a]] -> [a]
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 h :: forall a. [[a]] -> [a]
               at Main.hs:9:1-17
      Or perhaps ‘_module_Control_Monad’ is mis-spelled, or not in scope
    • In the expression: _module_Control_Monad
      In an equation for ‘h’: h = _module_Control_Monad
    • Relevant bindings include
        h :: [[a]] -> [a] (bound at Main.hs:10:1)
      Valid hole fits include
        h :: [[a]] -> [a]
        join :: forall (m :: * -> *) a. Monad m => m (m a) -> m a
        msum :: forall (t :: * -> *) (m :: * -> *) a.
                (Foldable t, MonadPlus m) =>
                t (m a) -> m a
        forever :: forall (f :: * -> *) a b. Applicative f => f a -> f b
   |
10 | h = _module_Control_Monad
   |     ^^^^^^^^^^^^^^^^^^^^^
```
