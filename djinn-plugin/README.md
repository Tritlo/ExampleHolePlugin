Djinn Plugin
=================


This hole plugin shows how `djinn` can be invoked by a hole fit plugin to synthesize simple programs.

`DjinnBridge` is based on the `djinn-ghc` package by Alejandro Serrano but modified to use `TcM` directly.

Note! Needs GHC 8.10

Example Output
-----------------

Using this plugin, you can compile the following:

```haskell
{-# OPTIONS -fplugin=DjinnPlugin -funclutter-valid-hole-fits #-}
module Main where

f :: a  -> a
f = _

g :: a -> b -> b
g = _

i :: (a,b) -> a
i = _

j :: a -> a -> a
j = _


main :: IO ()
main = return ()
```


And get the following output:

```
Main.hs:5:5: error:
    • Found hole: _ :: a -> a
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 f :: forall a. a -> a
               at Main.hs:4:1-12
    • In the expression: _
      In an equation for ‘f’: f = _
    • Relevant bindings include f :: a -> a (bound at Main.hs:5:1)
      Valid hole fits include
        (\ a -> a)
        (\ _ -> head (cycle (([]) ++ ([]))))
        (\ _ -> id (head (cycle (([]) ++ ([])))))
        f :: a -> a
        id :: forall a. a -> a
  |
5 | f = _
  |     ^

Main.hs:8:5: error:
    • Found hole: _ :: a -> b -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 g :: forall a b. a -> b -> b
               at Main.hs:7:1-16
    • In the expression: _
      In an equation for ‘g’: g = _
    • Relevant bindings include g :: a -> b -> b (bound at Main.hs:8:1)
      Valid hole fits include
        (\ _ a -> a)
        (\ _ a -> seq (head (cycle (([]) ++ ([])))) a)
        (\ _ a -> snd (head (cycle (([]) ++ ([]))), a))
        g :: a -> b -> b
        seq :: forall a b. a -> b -> b
  |
8 | g = _
  |     ^

Main.hs:11:5: error:
    • Found hole: _ :: (a, b) -> a
      Where: ‘b’, ‘a’ are rigid type variables bound by
               the type signature for:
                 i :: forall a b. (a, b) -> a
               at Main.hs:10:1-15
    • In the expression: _
      In an equation for ‘i’: i = _
    • Relevant bindings include
        i :: (a, b) -> a (bound at Main.hs:11:1)
      Valid hole fits include
        (\ (a, _) -> a)
        (\ _ -> head (cycle (([]) ++ ([]))))
        (\ _ -> f (head (cycle (([]) ++ ([])))))
        i :: (a, b) -> a
        fst :: forall a b. (a, b) -> a
   |
11 | i = _
   |     ^

Main.hs:14:5: error:
    • Found hole: _ :: a -> a -> a
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 j :: forall a. a -> a -> a
               at Main.hs:13:1-16
    • In the expression: _
      In an equation for ‘j’: j = _
    • Relevant bindings include
        j :: a -> a -> a (bound at Main.hs:14:1)
      Valid hole fits include
        (\ _ a -> a)
        (\ a _ -> a)
        (\ _ _ -> head (cycle (([]) ++ ([]))))
        j :: a -> a -> a
        g :: forall a b. a -> b -> b
        seq :: forall a b. a -> b -> b
        (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
   |
14 | j = _
   |     ^

```
