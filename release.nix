{ pkgs ? import <nixpkgs> }:
let
  spookPkgs = import ./default.nix {}; #{ inherit pkgs; };
  justStaticExecutables = pkgs.haskell.lib.justStaticExecutables;
in {
  backend = justStaticExecutables spookPkgs.ghc.backend;

  frontend = justStaticExecutables spookPkgs.ghcjs.frontend;

  frontend-static-files = pkgs.stdenv.mkDerivation {
    name = "frontend-static-files";
    src = ./frontend/public;
    installPhase = ''
      mkdir -p "$out/"
      cp -R ./ "$out/"
    '';
  };
}

