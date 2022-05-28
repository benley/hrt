{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc922" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    linear
    colour
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "hrt";
  buildInputs = [ ghc pkgs.zlib ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
