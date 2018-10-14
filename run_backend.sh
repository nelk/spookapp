#!/usr/bin/env bash

./result-backend/bin/backend-exe +RTS -N -RTS --serveIndexDirectory=./result-frontend-ghcjs/bin/frontend-exe.jsexe --serveStaticDirectory=./frontend/public --allowCrossOrigin --youtubeKey="$YOUTUBE_KEY" --youtubeSearchDelay=20

