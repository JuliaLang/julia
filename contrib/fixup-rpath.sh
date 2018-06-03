#!/bin/sh
# Usage: fixup-rpath.sh <patchelf path> <install libdir> <build libdir>

if [ $# -ne 3 ]; then
    echo "Incorrect number of arguments: Expected 3, got $#"
    echo "Usage: fixup-rpath.sh <patchelf path> <install libdir> <build libdir>"
    exit 1
fi

patchelf="$1"
install_libdir="$2"
build_libdir="$3"

for lib in ${install_libdir}/*.so*; do
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

    # Now set the new RPATH
    ${patchelf} --set-rpath "${new_rpath}" ${lib}
done
