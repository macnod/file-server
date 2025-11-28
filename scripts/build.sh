#!/bin/bash

SCRIPT_NAME=$(basename $0)
VERSION_FILE="version.txt"

source scripts/version.sh

function usage {
    echo "Usage: $SCRIPT_NAME [options]"
    echo 
    echo "This script builds the docker containers for file-server and"
    echo "and postgres, then uses Helm Charts to deploy to the specified"
    echo "environment. To build for the dev environment (default), you"
    echo "must be in branch dev. To build for the prod environment, you"
    echo "must be in branch master."
    echo
    echo "Options:"
    echo "  --env          The environment: prod or dev"
    echo "  --render-only  Don't build. Just create the rendered manifests"
    echo "                 file as it would have been deployed by help."
    echo "  --no-build     Deploy, but don't build."
    echo "  --no-cache     Don't use cache when building the container image."
    echo "  --no-deploy    Build the container image, but don't deploy."
    echo "  --push         Push the container to a container registry (CR)."
    echo "                 Must be logged into the CR."
    echo "  --verbose      Show docker build output."
    echo "  --help         Display this help screen."
    echo
    echo "Notes:"
    echo "  - If --no-cache is omitted, then 3rd party packages are cached, but local"
    echo "    packages that are in flux are not cached."
}

ENVIRONMENT="dev"
RENDER_ONLY=false
DEPLOY_ONLY=true
NO_BUILD=false
NO_CACHE=false
NO_DEPLOY=false
REDEPLOY=false
PUSH=false
VALUES_FILE=""
RENDERED_MANIFESTS=""
DOCKERFILE=""
LATEST=false
VERBOSE=false

load_version

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --env)
            shift
            ENVIRONMENT="$1"
            ;;
        --latest)
            LATEST=true
            ;;
        --render-only)
            RENDER_ONLY=true
            ;;
        --no-build)
            NO_BUILD=true
            ;;
        --no-cache)
            NO_CACHE=true
            ;;
        --no-deploy)
            NO_DEPLOY=true
            ;;
        --redeploy)
            NO_BUILD=true
            REDEPLOY=true
            ;;
        --push)
            PUSH=true
            ;;
        --verbose)
            VERBOSE=true
            ;;
        --help)
            usage
            exit 0
            ;;
    esac
    shift
done

# Branch check
CURRENT_BRANCH=$(git branch --show-current)
if [[ "$ENVIRONMENT" == "dev" ]]; then
    EXPECTED_BRANCH="dev"
else
    EXPECTED_BRANCH="master"
fi
if [[ "$CURRENT_BRANCH" != "$EXPECTED_BRANCH" ]]; then
    echo "Cannot deploy to '$ENVIRONMENT' from branch '$CURRENT_BRANCH'."
    echo "Expected branch '$EXPECTED_BRANCH'."
    exit 1
fi

# For --env, only dev or prod allowed
if [[ "$ENVIRONMENT" = "prod" ]]; then
    if [[ "$LATEST" = true ]]; then
        # We don't build for prod
        NO_BUILD=true
        copy_dev_to_prod
        VERSION="$DEV_VERSION"
    else
        # Redeploy last prod image (or dev if none)
        NO_BUILD=true
        if [[ "$PROD_VERSION" != "0.0.0" ]]; then
            VERSION="$PROD_VERSION"
        else
            VERSION="$DEV_VERSION"  # Fallback
        fi
    fi
elif [[ "$ENVIRONMENT" = "dev" ]]; then
    if [[ "$REDEPLOY" = false || "$NO_BUILD" = false ]]; then
        increment_version
    fi
    VERSION="$DEV_VERSION"
else
    usage
    echo "--env value must be 'dev' or 'prod'"
    exit 1
fi

# These depend on the environment
VALUES_FILE="charts/file-server/values-${ENVIRONMENT}.yaml"
RENDERED_MANIFESTS="${ENVIRONMENT}/kube/rendered-manifests.yaml"
DOCKERFILE="${ENVIRONMENT}/Dockerfile"
IMAGE="file-server"
RELEASE_NAME="file-server-${ENVIRONMENT}"

# Render the manifests
helm template $RELEASE_NAME ./charts/file-server \
    --namespace misc \
    --values "$VALUES_FILE" \
    --set image.tag="$VERSION" \
    > $RENDERED_MANIFESTS
echo "Rendered Kubernetes manifests to $RENDERED_MANIFESTS, with version $VERSION"
if [[ "$RENDER_ONLY" = true ]]; then
  exit 0
fi

# Build the containers, unless --no-build
if [[ "$NO_BUILD" = false ]]; then
    # Select the caching approach
    if [ "$NO_CACHE" = true ]; then
        CACHE_OPTION="--no-cache"
    else
        CACHE_OPTION="--build-arg CACHEBUST=$(date +%s)"
    fi
    # Select the right option for build output
    if [[ "$VERBOSE" = true ]]; then
        QUIET=""
    else
        QUIET="-q"
    fi
    docker build ${QUIET} -f "$DOCKERFILE" \
        $CACHE_OPTION \
        -t "macnod/${IMAGE}:${VERSION}" \
        -t "macnod/${IMAGE}:latest" \
        .
fi

# Deploy
if [[ "$NO_DEPLOY" = false ]]; then
    helm upgrade -install $RELEASE_NAME ./charts/file-server \
        --namespace misc \
        --create-namespace \
        --values "$VALUES_FILE" \
        --set image.tag=$VERSION
fi

# Push the containers
if [[ "$PUSH" = true ]]; then
    docker push "macnod/${IMAGE}:${VERSION}"
    docker push "macnod/${IMAGE}:latest"
fi

save_version
