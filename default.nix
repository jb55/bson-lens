{ nixpkgs ? import <nixpkgs> {}, compiler ? null }:
let pkgs = nixpkgs.pkgs;
    callPackage = if compiler != null
                    then pkgs.haskell.packages.${compiler}.callPackage
                    else pkgs.haskellPackages.callPackage;
in callPackage ./bson-lens.nix { }
