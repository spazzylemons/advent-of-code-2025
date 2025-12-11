{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      haskell.compiler.ghc910
      glpk
      zlib
    ];
  }
