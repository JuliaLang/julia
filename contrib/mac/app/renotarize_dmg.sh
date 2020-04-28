#!/bin/bash

# We need a URL
if [[ -z "$1" ]]; then
    echo "Usage: $0 <julia DMG url>" >&2
    exit 1
fi

# You need to define these in your environment
if [[ -z "${APPLEID}" ]] || [[ -z "${APPLEID_PASSWORD}" ]]; then
    echo "You must define APPLEID and APPLEID_PASSWORD in your environment!" >&2
    exit 1
fi

# Download .dmg
curl -L "$1" -O

# Unpack dmg into our `dmg` folder
rm -rf dmg

# Copy app over to our `dmg` folder
for j in /Volumes/Julia-*; do hdiutil detach "${j}"; done
hdiutil mount "$(basename "$1")"
cp -Ra /Volumes/Julia-* dmg

# Override some important Makefile variables
DMG_NAME=$(basename "$1")
APP_NAME=$(basename dmg/*.app)
VOL_NAME=$(basename /Volumes/Julia-*)
# Unmount everything again
for j in /Volumes/Julia-*; do hdiutil detach "${j}"; done

# Run notarization
make notarize "DMG_NAME=${DMG_NAME}" "APP_NAME=${APP_NAME}" "VOL_NAME=${VOL_NAME}"
