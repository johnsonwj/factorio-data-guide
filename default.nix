let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./factorio-data-guide.nix { }
