#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Codesign binary files for macOS.

usage() {
    echo "Usage: ${0} MACOS_CODESIGN_IDENTITY FILE-OR-DIRECTORY"
    exit 0
}

# Default codesign identity to `-` if not provided
if [ -z "${1}" ]; then
    MACOS_CODESIGN_IDENTITY="-"
    ENTITLEMENTS=""
else
    MACOS_CODESIGN_IDENTITY="${1}"
    ENTITLEMENTS="--entitlements $(dirname "${0}")/mac/app/Entitlements.plist"
fi

if [ "${#}" -eq 2 ]; then
    if [ -f "${2}" ]; then
        # Codesign only the given file
        MACHO_FILES="${2}"
    elif [ -d "${2}" ]; then
        # Find all files in the given directory
        MACHO_FILES=$(find "${2}" -type f -perm -0111 | cut -d: -f1)
    else
        usage
    fi
else
    usage
fi

echo "Codesigning with identity ${MACOS_CODESIGN_IDENTITY}"
for f in ${MACHO_FILES}; do
    echo "Codesigning ${f}..."
    codesign -s "${MACOS_CODESIGN_IDENTITY}" --option=runtime ${ENTITLEMENTS} -vvv --timestamp --deep --force "${f}"
done
