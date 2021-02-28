{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc901" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
