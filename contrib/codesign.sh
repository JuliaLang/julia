#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

set -euo pipefail

usage() {
    echo "Usage: $0 [--identity=<id>] [--keychain=<keychain_path>] <target>"
    echo
    echo "Parameter descriptions:"
    echo
    echo "             id: A codesigning identity hash or '-' to denote ad-hoc signing"
    echo "                 Default value is '-'"
    echo
    echo "  keychain_path: A path to a keychain file that contains the given identity"
    echo "                 If not given, uses the default keychain search path."
    echo
    echo "         target: A file or directory to codesign (must come last!)"
}

if [ "$#" -lt 1 ]; then
    usage
    exit 1
fi

# Default codesign identity to ad-hoc signing
CODESIGN_IDENTITY="-"

while [ "$#" -gt 1 ]; do
    case "${1}" in
        --identity)
            CODESIGN_IDENTITY="$2"
            shift
            shift
            ;;
        --identity=*)
            CODESIGN_IDENTITY="${1#*=}"
            shift
            ;;
        --keychain)
            KEYCHAIN_PATH="$2"
            shift
            shift
            ;;
        --keychain=*)
            KEYCHAIN_PATH="${1#*=}"
            shift
            ;;
        *)
            echo "Unknown argument '$1'"
            usage
            exit 1
            ;;
    esac
done

# Expand KEYCHAIN_PATH, in case it contains `~`:
KEYCHAIN_PATH="${KEYCHAIN_PATH/#\~/$HOME}"

# Verify keychain arg
KEYCHAIN_ARGS=""
if [ -n "${KEYCHAIN_PATH:-}" ]; then
    if [ -f "${KEYCHAIN_PATH}" ]; then
        KEYCHAIN_ARGS="--keychain ${KEYCHAIN_PATH}"

        # Ensure that the given keychain has already been added to our search list and is unlocked
        if ! security show-keychain-info "${KEYCHAIN_PATH}" >/dev/null 2>/dev/null; then
            echo "Must unlock keychain '${KEYCHAIN_PATH}' first!" >&2
            exit 1
        fi
    else
        echo "Given keychain file '${KEYCHAIN_PATH}' not found!" >&2
        exit 1
    fi
fi

# Verify that we can load the given identity
if [ "${CODESIGN_IDENTITY}" != "-" ]; then
    if ! security find-identity -p codesigning ${KEYCHAIN_PATH:-} 2>&1 | grep "${CODESIGN_IDENTITY}" >/dev/null; then
        echo "security find-identity found no matching identity for '${CODESIGN_IDENTITY}':"
        security find-identity -p codesigning ${KEYCHAIN_PATH:-}
        exit 1
    fi
fi

do_codesign() {
    # Codesign with the given identity, opting into the hardened runtime,
    # applying the entitlements plist we maintain within this repository,
    # asking for timestamping, performing "deep" codesigning for bundles,
    # and replacing any previous signatures that may exist.
    codesign --sign "${CODESIGN_IDENTITY}" \
             --option=runtime \
             --entitlements $(dirname "${0}")/mac/app/Entitlements.plist \
             ${KEYCHAIN_ARGS} \
             --timestamp \
             --deep \
             --force \
             "${1}"
}

if [ -f "${1}" ]; then
    # If we're codesigning a single file, directly invoke codesign on that file
    echo "Codesigning file ${1} with identity ${CODESIGN_IDENTITY}"
    do_codesign "${1}"
elif [ -d "${1}" ]; then
    # Create a fifo to communicate from `find` to `while`
    trap 'rm -rf $TMPFIFODIR' EXIT
    TMPFIFODIR="$(mktemp -d)"
    mkfifo "$TMPFIFODIR/findpipe"

    # If we're codesigning a whole directory, use `find` to discover every
    # executable file within the directory, then pass that off to a while
    # read loop.  This safely handles whitespace in filenames.
    find "${1}" -type f -perm -0111 -print0 > "$TMPFIFODIR/findpipe" &

    # This while loop reads in from the fifo, and invokes `do_codesign`,
    # but it does so in a background task, so that the codesigning can
    # happen in parallel.  This speeds things up by a few seconds.
    echo "Codesigning dir ${1} with identity ${CODESIGN_IDENTITY}"
    NUM_CODESIGNS=0
    while IFS= read -r -d '' exe_file; do
        do_codesign "${exe_file}" &
        NUM_CODESIGNS="$((NUM_CODESIGNS + 1))"
    done < "${TMPFIFODIR}/findpipe"
    wait
    echo "Codesigned ${NUM_CODESIGNS} files"
else
    echo "Given codesigning target '${1}' not a file or directory!" >&2
    usage
    exit 1
fi
