#!/usr/bin/env bash

nix-shell -j8 --attr shells.ghc --run "ghcid -c \"cabal new-repl frontend --ghc-options=-w\" --restart=common/src --reload=frontend/src -T Spook.App.Main.mainish"

