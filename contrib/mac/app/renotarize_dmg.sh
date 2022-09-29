#!/bin/bash
# This file is a part of Julia. License is MIT: https://julialang.org/license

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

# Use `aws` to download an `s3://` URL, otherwise use `curl`
URL="$1"
if [[ "$URL" == s3://* ]]; then
    aws s3 cp "${URL}" .
elif [[ "${URL}" == http* ]]; then
    # Download .dmg
    curl -L "${URL}" -O
else
    echo "Unknown URL format: '${URL}'" >&2
    exit 1
fi

# Unpack dmg into our `dmg` folder
rm -rf dmg
DMG_NAME=$(basename "${URL}")

# Copy app over to our `dmg` folder
for j in /Volumes/Julia-*; do hdiutil detach "${j}"; done
hdiutil mount "${DMG_NAME}"
cp -Ra /Volumes/Julia-* dmg

# Autodetect APP_NAME and VOL_NAME
APP_NAME=$(basename dmg/*.app)
VOL_NAME=$(basename /Volumes/Julia-*)

if [[ ! -d dmg/${APP_NAME} ]]; then
    echo "ERORR: Unable to auto-detect APP_NAME, check dmg folder!" >&2
    exit 1
fi
# Unmount everything again
for j in /Volumes/Julia-*; do hdiutil detach "${j}"; done

# Run notarization
make notarize "DMG_NAME=${DMG_NAME}" "APP_NAME=${APP_NAME}" "VOL_NAME=${VOL_NAME}"

# If it was an s3 bucket, auto-upload it
if [[ "${URL}" == s3://* ]]; then
    aws s3 cp --acl public-read "${DMG_NAME}" "${URL}"
fi
