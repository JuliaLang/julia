#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Usage: fixup-rpath.sh <patchelf path> <dir to process> <build libdir>

if [ $# -ne 3 ]; then
    echo "Incorrect number of arguments: Expected 3, got $#"
    echo "Usage: fixup-rpath.sh <patchelf path> <directory to process> <build libdir>"
    exit 1
fi

patchelf="$1"
executable_dir="$2"
build_libdir="$3"

for lib in $(find ${executable_dir} -type f -perm -111); do
    # First get the current RPATH
    rpath="$(${patchelf} --print-rpath ${lib})"

    # If it doesn't contain the build's libdir, we don't care about it
    if [ -z "$(echo ${rpath} | grep -F ${build_libdir})" ]; then
        continue
    fi

    # Remove build_libdir from the RPATH, retaining the rest
    new_rpath="$(echo ${rpath} | tr : \\n | grep -vF ${build_libdir} | tr \\n :)"
    # Drop the trailing :
    new_rpath="${new_rpath%?}"

    echo "  Setting RPATH for ${lib} to '${new_rpath}'"

    # Now set the new RPATH
    ${patchelf} --set-rpath "${new_rpath}" ${lib}
done
