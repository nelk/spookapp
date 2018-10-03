Spook.app
==========

## Development Tool Setup.
// System tools:
postgresql
nix-env -i nixops

// Use vim ghcid plugin: https://github.com/ndmitchell/ghcid.
./develop.sh common common

## Development DB Setup (see deploy\_logical.nix)
> sudo -u postgres psql template1
> > ALTER USER postgres with encrypted password 'xxxxxxx';

> sudo vim /etc/postgresql/9.1/main/pg\_hba.conf
> # Change "peer" to "md5" on the line concerning postgres :
> # local      all     postgres     md5

> psql postgres
> DATABASE spook;
> USER spookapp WITH PASSWORD 'password';
> ALL ON DATABASE spook TO spookapp;

## Building.
> ./build\_backend.sh
> ./build\_frontend\_ghcjs.sh

## Running in local dev mode
Two ways to run:
1. Ghc - run a jsaddle-warp server for the frontend on :3003 (frontend run as native code with websocket control of browser apis), backend on :8080. Backend accepts x-origin xhr and serves static content.
2. Ghcjs - compile frontend to js and run only backend server on :8080. Backend serves both static content and compiled app/index files.

> ./build\_backend.sh
> ./run\_backend.sh

On separate shell:
> ./run\_frontend\_autoreload.sh

## Running in production mode
Frontend always compiled with ghcjs. Backend server rejects x-origin xhr and does not serve either static content or compiled app/index files.
Nginx is used to serve all static content and proxy api calls to backend.
Two physical deployment options listed below.

> ./build\_frontend\_ghcjs.sh
> ./build\_backend.sh
> ./run\_backend.sh

## Local deployment in production mode (virtualbox):
> nixops create deploy\_logical.nix deploy\_vbox.nix --name spook-vbox
> nixops deploy -d spook-vbox
> // Restart/reset both machines from virtualbox UI. TODO - fix this.
> nixops deploy -d spook-vbox
> // Ssh into database and run 'systemctl restart postgresql'. TODO - fix this.
> // Ssh into webserver and run 'systemctl restart spook'

## Cloud production deployment (GCE):
> // Copy/symlink pkey.pem to project directory.
> // Create .envrc that exports environment variables GCE\_PROJECT, GCE\_SERVICE\_ACCOUNT, and ACCESS\_KEY\_PATH.
> source .envrc
> nixops create deploy\_logical.nix deploy\_gce.nix --name spook-gce
> nixops deploy -d spook-gce

## Debugging running production servers (GCE or virtualbox):
> nixops check -d spook-vbox
> nixops ssh -d spook-vbox webserver
> > systemctl --failed
> > journalctl -u spook

