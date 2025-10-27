#!/bin/bash

SCRIPT_NAME=$(basename $0)

function usage {
    echo "Usage: $SCRIPT_NAME [options]"
    echo "Options:"
    echo "  --version   The version of the image to build, like 0.12."
    echo "  --no-cache  Don't use the cache at all when building the image"
    echo "  --help      Display this help screen"
    echo "Notes:"
    echo "  - If --no-cache is omitted, then 3rd party packages are cached,"
    echo "    but local packages that are in flux are not cached."
    if [ "$HELP" = true ]; then
        exit 0
    else
        exit 1
    fi
}

VERSION=""
NO_CACHE=false
HELP=false

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
        --help)
            HELP=true
            usage
            ;;
        *)
            usage
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
