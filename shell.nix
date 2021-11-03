{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc921" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
