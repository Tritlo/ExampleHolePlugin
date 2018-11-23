ExampleHolePlugin
=================

An example of a hole fit plugin for GHC that can filter by module and 
searches the local Hoogle for fits (if hoogle is available, and the 
`-fplugin-opt=HolePlugin:hoogle` is set).

make sure that `hoogle` is installed (for demo of hoogle features)
and that the local hoogle database has been generated (with `hoogle generate`)

then, build with

```
  cabal new-build test
```

and modify `test/Main.hs` to try it out (or run it on your own files)
