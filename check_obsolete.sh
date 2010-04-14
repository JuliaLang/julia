#!/bin/bash
target=julia-debug
for file in $(git ls-files      |\
  grep -v '\.s$'                |\
  grep -v '\.j$'                |\
  grep -v '\.el$'               |\
  grep -v '\.gitignore$'        |\
  grep -v '^doc/'               |\
  grep -v '^lib/'               |\
  grep -v 'Make.inc'            |\
  grep -v 'Makefile'            |\
  grep -v '^check_obsolete.sh$' )
do
  make >/dev/null 2>/dev/null
  if test ! -f $target; then
    echo -e "REBUILDING...\n" >&2
    make cleanall >/dev/null
    make >/dev/null 2>/dev/null
    test -f $target || exit
  fi
  echo -e "TRYING $file...\n" >&2
  rm -f $target
  mv $file $file.save
  touch $file
  make >/dev/null
  if test ! -f $target; then
    mv $file.save $file
    make cleanall >/dev/null
    mv $file $file.save
    make >/dev/null
    if test -f $target; then
      echo -e "\nOBSOLETE: $file\n" >&2
      echo $file
    else
      echo -e "\nNECESSARY: $file\n" >&2
    fi
  else
    echo -e "\nNECESSARY: $file\n" >&2
  fi
  mv $file.save $file
  touch $file
done
