#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

PLATFORM="$1"

cat "$SCRIPT_DIR/$PLATFORM.arches" | tr -s ' ' | while read _line; do
  # Remove whitespace from the beginning and end of each line
  line=`echo $_line | tr -s ' '`

  # Skip all lines that begin with `#`
  if [[ $line == \#* ]]; then
    continue
  fi

  export ARCH=`echo $line        | cut -d ' ' -f 1`
  export ARCH_LABEL=`echo $line  | cut -d ' ' -f 2`
  export ROOTFS_ARCH=`echo $line | cut -d ' ' -f 3`
  export TIMEOUT=`echo $line     | cut -d ' ' -f 4`
  export ROOTFS_TAG=`echo $line  | cut -d ' ' -f 5`
  export ROOTFS_TREE=`echo $line | cut -d ' ' -f 6`
  echo "Launching: $PLATFORM $ARCH $ARCH_LABEL $ROOTFS_ARCH $TIMEOUT"
  buildkite-agent pipeline upload "$SCRIPT_DIR/$PLATFORM.yml"
done
