{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc921" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./pwdhash.nix {}
