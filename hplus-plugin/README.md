# Hoogle Plus Plugin

This plugin requires you to give an argument to the plugin to where hoogle_plus
has been installed (and the generate command already run). It then send the
type to hplus and returns the result as the result. Note that it is very
brittle, and only works with

``` sh
$ cabal new-build Test/
```

at the moment.

See https://github.com/davidmrdavid/hoogle_plus for instructions on how to setup
hoogle plus.
