#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Loop over all command line arguments
for i in "$@"; do
    # If an argument starts with -L, echo it out sans -L!
    if [[ $i == -L* ]]; then
        echo "\"${i:2:${#i}}\""
    fi
done
