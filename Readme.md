Spook.app
==========

## Development Setup.
// System tools:
postgresql
nix-env -i nixops

// Use vim ghcid plugin: https://github.com/ndmitchell/ghcid.
./develop.sh common common

## DB Setup (see deploy\_logical.nix)
> psql postgres
> DATABASE spook;
> USER spookapp WITH PASSWORD 'password';
> ALL ON DATABASE spook TO spookapp;

## Building.
> ./build\_backend.sh
> ./build\_frontend\_ghcjs.sh

## Running in dev mode
> TODO

## Local deploy (virtualbox):
> nixops create deploy\_logical.nix deploy\_vbox.nix --name spook-vbox
> nixops deploy -d spook-vbox
> // Restart/reset both machines from virtualbox UI. TODO - fix this.
> nixops deploy -d spook-vbox
> // Ssh into database and run 'systemctl restart postgresql'. TODO - fix this.
> // Ssh into webserver and run 'systemctl restart spook'

## Production deploy (GCE):
> // Copy/symlink pkey.pem to project directory.
> nixops create deploy\_logical.nix deploy\_gce.nix --name spook-gce
> nixops deploy -d spook-gce

## Debugging running servers:
> nixops check -d spook-vbox
> nixops ssh -d spook-vbox webserver
> > systemctl --failed
> > journalctl -u spook

