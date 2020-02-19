# Hoogle Plus Plugin

This plugin requires you to give an argument to the plugin to where hoogle_plus
has been installed (and the generate command already run). It then send the
type to hplus and returns the result as the result. Note that it is very
brittle, and only works with

``` sh
$ cabal new-build Test/
```
at the moment.

This prints out:

``` sh
Main.hs:6:5: error:
    • Found hole: _ :: (a -> b, a) -> b
      Where: ‘a’, ‘b’ are rigid type variables bound by
               the type signature for:
                 t :: forall a b. (a -> b, a) -> b
               at Main.hs:5:1-23
    • In the expression: _
      In an equation for ‘t’: t = _
    • Relevant bindings include
        t :: (a -> b, a) -> b (bound at Main.hs:6:1)
      Valid hole fits include (Data.Function.$) (fst arg0) (snd arg0)
  |
6 | t = _
  |     ^
```

See https://github.com/davidmrdavid/hoogle_plus for instructions on how to setup
hoogle plus.
