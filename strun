#!/bin/bash

# Trigger a sync with Syncthing

if [ -f .env ]; then
    export $(cat .env | xargs)
fi

if [ -z "$SYNCTHING_API_KEY" ]; then
    echo "Error: SYNCTHING_API_KEY is not set."
    exit 1
fi

# Run the sync command safely
curl -X POST -H "X-API-Key: $SYNCTHING_API_KEY" \
     "http://localhost:8384/rest/db/scan?folder=$SYNCTHING_FOLDER_ID"
