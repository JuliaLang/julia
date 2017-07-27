#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Loop over all command line arguments
for i in "$@"; do
    # If an argument starts with -L, echo it out sans -L!
    case $i in
    -L*) printf '"%s"\n' "${i#-L}" ;;
    esac
done
