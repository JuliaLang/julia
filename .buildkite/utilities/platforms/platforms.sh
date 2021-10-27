#!/bin/bash

ARCHES="$1"
YAML="$2"

if [[ ! -f "${ARCHES:?}" ]] ; then
  echo "File does not exist: ${ARCHES:?}"
  exit 1
fi

if [[ ! -f "${YAML:?}" ]] ; then
  echo "File does not exist: ${YAML:?}"
  exit 1
fi

cat "${ARCHES:?}" | tr -s ' ' | while read _line; do
  # Remove whitespace from the beginning and end of each line
  line=`echo $_line | tr -s ' '`

  # Skip all lines that begin with `#`
  if [[ $line == \#* ]]; then
    continue
  fi

  export PLATFORM=`echo $line    | cut -d ' ' -f 1  | tr -s ' '`
  export LABEL=`echo $line       | cut -d ' ' -f 2  | tr -s ' '`
  export ALLOW_FAIL=`echo $line  | cut -d ' ' -f 3  | tr -s ' '`
  export ARCH=`echo $line        | cut -d ' ' -f 4  | tr -s ' '`
  export ARCH_ROOTFS=`echo $line | cut -d ' ' -f 5  | tr -s ' '`
  export MAKE_FLAGS=`echo $line  | cut -d ' ' -f 6  | tr -s ' '`
  export TIMEOUT=`echo $line     | cut -d ' ' -f 7  | tr -s ' '`
  export IS_RR=`echo $line       | cut -d ' ' -f 8  | tr -s ' '`
  export IS_ST=`echo $line       | cut -d ' ' -f 9  | tr -s ' '`
  export IS_MT=`echo $line       | cut -d ' ' -f 10 | tr -s ' '`
  export ROOTFS_TAG=`echo $line  | cut -d ' ' -f 11 | tr -s ' '`
  export ROOTFS_HASH=`echo $line | cut -d ' ' -f 12 | tr -s ' '`

  if [[ "${MAKE_FLAGS:?}" == "none" ]]; then
    export MAKE_FLAGS=""
  fi

  buildkite-agent pipeline upload "${YAML:?}"
done
