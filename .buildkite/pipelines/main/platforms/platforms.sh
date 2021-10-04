#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

OS="$1"

cat "$SCRIPT_DIR/$OS.arches" | tr -s ' ' | while read _line; do
  # Remove whitespace from the beginning and end of each line
  line=`echo $_line | tr -s ' '`

  # Skip all lines that begin with `#`
  if [[ $line == \#* ]]; then
    continue
  fi

  export PLATFORM=`echo $line    | cut -d ' ' -f 1`
  export ARCH=`echo $line        | cut -d ' ' -f 2`
  export ARCH_LABEL=`echo $line  | cut -d ' ' -f 3`
  export ROOTFS_ARCH=`echo $line | cut -d ' ' -f 4`
  export ALLOW_FAIL=`echo $line   | cut -d ' ' -f 5`
  export TIMEOUT=`echo $line     | cut -d ' ' -f 6`
  export ROOTFS_TAG=`echo $line  | cut -d ' ' -f 7`
  export ROOTFS_TREE=`echo $line | cut -d ' ' -f 8`
  echo "Launching: $OS $PLATFORM $ARCH $ARCH_LABEL $ROOTFS_ARCH $ALLOW_FAIL $TIMEOUT"
  buildkite-agent pipeline upload "$SCRIPT_DIR/$OS.yml"
done
