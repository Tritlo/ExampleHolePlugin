ExampleHolePlugin
=================

This repository contains a collection of example hole fit plugins for GHC.

Note! Needs to a custom branch of GHC [currently in submission](https://gitlab.haskell.org/ghc/ghc/merge_requests/153).

Checkout the plugins in the directories:

+ The [Hoogle Plugin](hoogle-plugin/) shows an example of how you can filter by candidates by module, but also how you can interact with command line tools such as Hoogle.
+ The [Djinn Plugin](djinn-plugin/) show an example how state can be used to communicate between the candidate and fit plugin and between invocations, by using `djinn` to synthesize hole fits. Based on `djinn-ghc` by Alejandro Serrano.
+ The [QuickCheck Plugin](quickcheck-plugin/) shows how hole fit plugins can be used to automatically pick the right hole fit based on QuickCheck properties.
