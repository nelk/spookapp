#!/usr/bin/env bash

nix-build -A ghcjs.frontend -j8 -o result-frontend-ghcjs

