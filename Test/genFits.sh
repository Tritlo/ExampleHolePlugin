#!/usr/bin/env bash

cp Main.hs MainOrig.hs
rm -f out.fits
echo '\n' | ( echo ":load FindFit.hs" \
            && echo "FindFit.main" \
            && echo ":load FitTest.hs"\
            && echo "FitTest.main"\
            && echo ":q" \
            && cat ) | cabal new-repl
bat --theme='Monokai Extended Light' Main.hs
cp MainOrig.hs Main.hs


