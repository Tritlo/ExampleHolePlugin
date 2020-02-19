let
  unstable = import <unstable> { };
in
{ nixpkgs ? import <nixpkgs> { } }:
with nixpkgs;
let
  hspkgs = unstable.haskell.packages.ghc8101;
  pkgs = [
    hspkgs.ghc
  ];

in nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = pkgs;
}
