#!/bin/sh
# print arguments escaped as json list elements

first=true
for n in "$@"; do
  $first && first=false || printf ', '
  n=$(printf '%s' "$n" | sed 's/\\/\\\\/g' | sed 's/"/\\"/g')
  printf '"%s"' "$n"
done
