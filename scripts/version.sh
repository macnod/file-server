#!/bin/bash

# ... (keep existing header)

# Now track two versions
DEV_VERSION=""
PROD_VERSION=""

check_version() {
    if [[ ! "$1" =~ ^[0-9]{1,2}\.[0-9]{1,2}\.[0-9]{1,2}$ ]]; then
        echo "ERROR: Invalid version format: '$1'." >&2
        echo "       Expected format: X.Y.Z (e.g., 1.23.456)" >&2
        return 1
    fi
}

# Load versions from version.txt (first line: dev, second: prod)
load_version() {
    if [[ -f "$VERSION_FILE" && -r "$VERSION_FILE" ]]; then
        mapfile -t VERSIONS < "$VERSION_FILE"
        DEV_VERSION="${VERSIONS[0]:-0.0.0}"
        PROD_VERSION="${VERSIONS[1]:-0.0.0}"
        if ! check_version "$DEV_VERSION" || ! check_version "$PROD_VERSION"; then
            echo "ERROR: Invalid version in $VERSION_FILE." >&2
            return 1
        fi
    else
        echo "Version file '$VERSION_FILE' not found. Defaulting to 0.0.0 for both."
        DEV_VERSION="0.0.0"
        PROD_VERSION="0.0.0"
    fi
}

# Increment only DEV_VERSION
increment_version() {
    if ! check_version "$DEV_VERSION"; then
        echo "ERROR: Cannot increment invalid dev version: '$DEV_VERSION'" >&2
        return 1
    fi
    local major minor patch
    declare -i major minor patch
    IFS='.' read -r major minor patch <<< "$DEV_VERSION"
    (( ++patch ))
    if (( patch >= 100 )); then
        patch=0
        (( ++minor ))
        if (( minor >= 100 )); then
            minor=0
            (( ++major ))
        fi
    fi
    DEV_VERSION="${major}.${minor}.${patch}"
}

# Copy dev to prod (for --latest)
copy_dev_to_prod() {
    PROD_VERSION="$DEV_VERSION"
}

# Save both versions
save_version() {
    if ! check_version "$DEV_VERSION" || ! check_version "$PROD_VERSION"; then
        echo "ERROR: Refusing to save invalid versions." >&2
        return 1
    fi
    printf "%s\n%s\n" "$DEV_VERSION" "$PROD_VERSION" > "$VERSION_FILE"
    echo "Versions updated: Dev=$DEV_VERSION, Prod=$PROD_VERSION → $VERSION_FILE"
}

# Global VERSION for backward compatibility (set to dev by default)
VERSION="$DEV_VERSION"
