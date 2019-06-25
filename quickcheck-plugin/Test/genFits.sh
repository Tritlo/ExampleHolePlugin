#!/usr/bin/env bash

cp Main.hs MainOrig.hs
rm -f out.fits
echo '\n' | ( echo ":load FindFit.hs" \
            && echo "Test.FindFit.genFitTestModule" \
            && echo ":load FitTest.hs"\
            && echo "FitTest.executeFitTest"\
            && echo ":q" \
            && cat ) | cabal new-repl --allow-newer
bat --theme='Monokai Extended Light' Main.hs
cp MainOrig.hs Main.hs


