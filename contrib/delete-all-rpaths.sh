#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

[ "$(uname)" = Darwin ] || { echo "Requires Darwin." 2>&1; exit 1; }

if [ $# -lt 1 ]; then
  echo "usage: $(basename $0) lib1 [lib2 ...]" 2>&1
  exit 1
fi

LIBS=''

# filter out symlinks
for lib in "$@" ; do
  if [ ! -L "$lib" ]; then
    LIBS="$LIBS $lib"
  fi
done

# regex to match and capture the path in a LC_RPATH as output by `otool -l`.
rpath_r="^ +path ([[:print:]]+) \(offset [[:digit:]]+\)$"

for lib in $LIBS ; do
  # Find the LC_RPATH commands, with two lines of trailing
  # context, and for each line look for the path to delete it.
  otool -l "$lib" | grep LC_RPATH -A2 |
  while IFS='' read -r line || [ -n "$line" ]; do
    if [[ $line =~ $rpath_r ]]; then
      echo $(basename $lib) deleting rpath "${BASH_REMATCH[1]}" \""$lib"\"
      install_name_tool -delete_rpath "${BASH_REMATCH[1]}" "$lib"
    fi
  done
done
