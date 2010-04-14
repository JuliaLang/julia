#!/bin/bash
target=julia-debug
for file in $(git ls-files            |\
              grep -v '\.s$'          |\
              grep -v '\.j$'          |\
              grep -v '\.el$'         |\
              grep -v '\.gitignore$'  |\
              grep -v '^doc/'         |\
              grep -v '^lib/'         |\
              grep -v 'Make.inc'      |\
              grep -v 'Makefile'      )
do
  echo "Trying $file..." >&2
  rm -f $target
  cat /dev/null >$file
  make >/dev/null
  if test -f $target; then
    git checkout -f >/dev/null
    make cleanall >/dev/null
    rm -f $file
    make >/dev/null
    if test -f $target; then
      echo "OBSOLETE: $file" >&2
      echo $file
    fi
  fi
  git checkout -f >/dev/null
  make >/dev/null
  if test ! -f $target; then
    make cleanall >/dev/null
    make >/dev/null
    test -f $target || exit
  fi
  echo >&2
done
