#!/usr/bin/env bash

if [ "$#" -ne 2 ];
then printf "./develop.sh (package) (package target)\nEx. ./develop.sh common spec\n"; exit 1;
fi

# TODO: Make this work - doesn't seem to restart at all.
if [ "${2}" == "spec" ];
then extra_ghcid_flags="--restart=${1}/${1}.cabal --restart=${1}/src --reload=${1}/test";
else extra_ghcid_flags="--restart=${1}/${1}.cabal --reload=${1}/src"
fi

# "--ghc-options=-fno-code" does not seem to work.
# nix-shell -j8 --attr shells.ghc --command "nvim -c \":Ghcid --command 'cabal new-repl ${1}'\" ./common/Setup.hs"

# TODO: Figure out how to nest one level deeper :(
# ghc_options="-Wall -Wno-unused-imports -Wno-warn-orphans"
ghcid_command="cabal new-repl ${1}:${2}"
nvim_command=":Ghcid --command '${ghcid_command}' --directory=../ ${extra_ghcid_flags}"
nix_command="cd ${1} && COLORTERM=gnome-terminal nvim -c \"${nvim_command}\" ./Setup.hs"

nix-shell -j8 --attr shells.ghc --command "${nix_command}"


