#!/bin/bash

libnames=""
libpaths=""

# Loop over all command line arguments
for i in "$@"; do
    # Get basename of this argument, check if it is already in libnames
    name=$(basename "$i")

    if [[ -z $(echo $libnames | grep "$name") ]]; then
        libnames+="$name "
        libpaths+="$i "
    fi
done

echo $libpaths
