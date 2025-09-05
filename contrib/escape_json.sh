#!/bin/bash
# print arguments escaped as json list elements

first=true
for n in "$@"; do
  $first && first=false || printf ', '
  n="${n//\\/\\\\}"
  n="${n//\"/\\\"}"
  printf '"%s"' "$n"
done
