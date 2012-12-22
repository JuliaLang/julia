#!/bin/sh

# Loop over all command line arguments
for i in "$@"; do
    # If an argument starts with -L, echo it out sans -L!
    if [[ $i == -L* ]]; then
        echo "\"${i:2:${#i}}\""
    fi
done
