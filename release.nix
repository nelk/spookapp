{ pkgs ? import <nixpkgs> }:
let
  spookPkgs = import ./default.nix {}; #{ inherit pkgs; };
  justStaticExecutables = pkgs.haskell.lib.justStaticExecutables;
in {
  backend = justStaticExecutables spookPkgs.ghc.backend;
  frontend = justStaticExecutables spookPkgs.ghcjs.frontend;
}

