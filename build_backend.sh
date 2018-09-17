#!/usr/bin/env bash

nix-build -A ghc.backend -j8 -o result-backend

