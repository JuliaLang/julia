#!/bin/bash

ARCHES="$1"
YAML="$2"

if [[ ! -f "${ARCHES:?}" ]] ; then
  echo "Arches file does not exist: ${ARCHES:?}"
  exit 1
fi

if [[ ! -f "${YAML:?}" ]] ; then
  echo "YAML file does not exist: ${YAML:?}"
  exit 1
fi

cat "${ARCHES:?}" | tr -s ' ' | while read _line; do
  # Remove whitespace from the beginning and end of each line
  line=`echo $_line | tr -s ' '`

  # Skip any line that begins with the `#` character
  if [[ $line == \#* ]]; then
    continue
  fi

  # Skip any empty line
  if [[ $line == "" ]]; then
    continue
  fi

  export PLATFORM=`echo $line    | cut -d ' ' -f 1  | tr -s ' '`
  export LABEL=`echo $line       | cut -d ' ' -f 2  | tr -s ' '`
  export GROUP=`echo $line       | cut -d ' ' -f 3  | tr -s ' '`

  export ALLOW_FAIL=`echo $line  | cut -d ' ' -f 4  | tr -s ' '`
  export ARCH=`echo $line        | cut -d ' ' -f 5  | tr -s ' '`
  export ARCH_ROOTFS=`echo $line | cut -d ' ' -f 6  | tr -s ' '`

  export MAKE_FLAGS=`echo $line  | cut -d ' ' -f 7  | tr -s ' '`
  export TIMEOUT_BK=`echo $line  | cut -d ' ' -f 8  | tr -s ' '`
  export TIMEOUT_RR=`echo $line  | cut -d ' ' -f 9  | tr -s ' '`
  export RETRIES=`echo $line     | cut -d ' ' -f 10 | tr -s ' '`
  export IS_RR=`echo $line       | cut -d ' ' -f 11 | tr -s ' '`
  export IS_ST=`echo $line       | cut -d ' ' -f 12 | tr -s ' '`
  export IS_MT=`echo $line       | cut -d ' ' -f 13 | tr -s ' '`
  export ROOTFS_TAG=`echo $line  | cut -d ' ' -f 14 | tr -s ' '`
  export ROOTFS_HASH=`echo $line | cut -d ' ' -f 15 | tr -s ' '`

  if [[   "${IS_ST:?}"   == "yes" ]]; then
    if [[ "${IS_MT:?}"   == "yes" ]]; then
      echo "You cannot set both IS_ST and IS_MT to yes"
      exit 1
    fi
  fi

  if [[ "${ALLOW_FAIL:?}" == "." ]]; then
    export ALLOW_FAIL="false"
  fi

  if [[ "${MAKE_FLAGS:?}" == "." ]]; then
    export MAKE_FLAGS=""
  fi

  if [[ "${TIMEOUT_BK:?}" == "." ]]; then
    export TIMEOUT_BK="60"
  fi

  if [[ "${TIMEOUT_RR:?}" == "." ]]; then
    export TIMEOUT_RR="30"
  fi

  if [[ "${RETRIES:?}" == "." ]]; then
    export RETRIES="0"
  fi

  buildkite-agent pipeline upload "${YAML:?}"
done
