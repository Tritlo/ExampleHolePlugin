#!/usr/bin/env bash

(echo ":load out.fits.hs" && echo "FitTest.main" && cat) | cabal new-repl


