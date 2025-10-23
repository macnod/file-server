#!/bin/bash

VERSION=""
NO_CACHE=false

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --version)
            shift
            VERSION="$1"
            ;;
        --no-cache)
            NO_CACHE=true
            ;;
        *)
            # Ignore other arguments
            ;;
    esac
    shift
done

# Check if VERSION is empty
if [ -z "$VERSION" ]; then
    echo "Error: Version is required. Use --version <major>.<minor>"
    exit 1
fi

if [ "$NO_CACHE" = true ]; then
    docker build --no-cache \
           -t "macnod/file-server:$VERSION" \
           -t "macnod/file-server:latest" .
else
    docker build --build-arg CACHEBUST=$(date +%s) \
           -t "macnod/file-server:$VERSION" \
           -t "macnod/file-server:latest" .
fi
