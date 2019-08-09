Hoogle Plugin
=================

Note! Needs a freshly built GHC HEAD

An example of a hole fit plugin for GHC that can filter by module and
searches the local Hoogle for fits (if hoogle is available, and the
`-fplugin-opt=HolePlugin:hoogle` is set).

make sure that `hoogle` is installed (for demo of hoogle features)
and that the local hoogle database has been generated (with `hoogle generate`)

then, build with

```
  cabal new-build test
```

and modify `test/Main.hs` to try it out (or run it on your own files).



Example Output:
---------------


When run on:

```haskell
{-# OPTIONS -fplugin=HolePlugin -fplugin-opt=HolePlugin:hoogle #-}
-- Make sure to remove the hoogle opt if hoogle is not available locally
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)

t :: [Int] -> Int
t = _module_Prelude

g :: [Int] -> Int
g = _module_Data_List

main :: IO ()
main = print $ t [1,2,3]
```

the output is:

```
Main.hs:14:5: error:
    • Found hole: _module_Prelude :: [Int] -> Int
      Or perhaps ‘_module_Prelude’ is mis-spelled, or not in scope
    • In the expression: _module_Prelude
      In an equation for ‘t’: t = _module_Prelude
    • Relevant bindings include
        t :: [Int] -> Int (bound at Main.hs:14:1)
      Valid hole fits include
        Hoogle says: GHC.List length :: [a] -> Int
        Hoogle says: GHC.OldList length :: [a] -> Int
        t :: [Int] -> Int (bound at Main.hs:14:1)
        g :: [Int] -> Int (bound at Main.hs:17:1)
        length :: forall (t :: * -> *) a. Foldable t => t a -> Int
          with length @[] @Int
          (imported from ‘Prelude’ at Main.hs:5:1-34
           (and originally defined in ‘Data.Foldable’))
        maximum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
          with maximum @[] @Int
          (imported from ‘Prelude’ at Main.hs:5:1-34
           (and originally defined in ‘Data.Foldable’))
        (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
   |
14 | t = _module_Prelude
   |     ^^^^^^^^^^^^^^^

Main.hs:17:5: error:
    • Found hole: _module_Data_List :: [Int] -> Int
      Or perhaps ‘_module_Data_List’ is mis-spelled, or not in scope
    • In the expression: _module_Data_List
      In an equation for ‘g’: g = _module_Data_List
    • Relevant bindings include
        g :: [Int] -> Int (bound at Main.hs:17:1)
      Valid hole fits include
        Hoogle says: GHC.List length :: [a] -> Int
        Hoogle says: GHC.OldList length :: [a] -> Int
        g :: [Int] -> Int (bound at Main.hs:17:1)
        head :: forall a. [a] -> a
          with head @Int
          (imported from ‘Data.List’ at Main.hs:7:19-22
           (and originally defined in ‘GHC.List’))
        last :: forall a. [a] -> a
          with last @Int
          (imported from ‘Data.List’ at Main.hs:7:25-28
           (and originally defined in ‘GHC.List’))
   |
17 | g = _module_Data_List
```
