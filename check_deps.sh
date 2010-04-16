#!/bin/bash

target=${1:-julia-debug}

make "$target" >/dev/null 2>/dev/null
if test ! -f "$target"; then
  make cleanall
  make $target
  if test ! -f "$target"; then
    echo "ERROR: couldn't make $target"
    exit
  fi
fi

for file in $(git ls-files | grep -P '\.(h|c|cpp|s|scm)$' | grep -Pv '^lib/'); do
  sleep 1
  touch "$file"
  make $target >/dev/null 2>/dev/null
  if test "$target" -nt "$file"; then
    echo "DEPENDENCY: $file"
  else
    echo "NON-DEPENDENCY: $file"
  fi
done
