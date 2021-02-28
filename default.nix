{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc901" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./inventory.nix { }
