#!/usr/bin/env bash
rm test/out.fits
echo "[" >> test/out.fits
cabal new-build test
echo "]" >> test/out.fits
cat test/out.fits


