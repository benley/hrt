{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902" }:
let
  inherit (nixpkgs) pkgs;
  haskellPackages = pkgs.haskell.packages.${compiler};

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    aeson
    linear
    colour
    JuicyPixels
    lens
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "hrt";
  buildInputs = [
    ghc
    pkgs.zlib
    haskellPackages.hoogle
  ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
