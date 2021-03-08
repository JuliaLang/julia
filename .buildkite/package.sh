#!/usr/bin/env bash

set -eux

case "$ARCH" in
  armv7l)
    MAKEFLAGS+=("-DLLVM_HOST_TRIPLE=armv7l-unknown-linux-gnueabihf")
    MAKEFLAGS+=("-DLLVM_DEFAULT_TARGET_TRIPLE=armv7l-unknown-linux-gnueabihf")
    ;;
esac

"$MAKE" cleanall
"$MAKE" release
"$MAKE" build-stats
"$MAKE" binary-dist

# if [[ "$IS_MASTER" == "true" ]]; then
#   case "$OS" in
#     mac)
#       .buildkite/unlock_keychain.sh
#       "$MAKE" -j1 app  # This target doesn't like running with multiple threads.
#       ;;
#   esac
# fi

eval "$($MAKE print-JULIA_BINARYDIST_FILENAME)"

buildkite-agent meta-data set "package-$OS-$ARCH" "$JULIA_BINARYDIST_FILENAME.$PKG_EXT"
buildkite-agent artifact upload "$JULIA_BINARYDIST_FILENAME.$PKG_EXT"
