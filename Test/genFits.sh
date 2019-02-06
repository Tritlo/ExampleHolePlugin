#!/usr/bin/env bash

cp Main.hs MainOrig.hs
rm -f out.fits
echo '\n' | ( echo ":load FindFit.hs" \
            && echo "main" \
            && echo ":load FitTest.hs"\
            && echo "FitTest.main"\
            && echo ":q" \
            && cat ) | cabal new-repl
cat Main.hs
diff MainOrig.hs Main.hs
cp MainOrig.hs Main.hs


