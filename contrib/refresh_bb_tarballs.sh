#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Invoke this with no arguments to refresh all tarballs, or with a project name to refresh only that project.
#
# Example:
#   ./refresh_bb_tarballs.sh gmp

# Get this list via:
#    using BinaryBuilder
#    print("TRIPLETS=\"$(join(triplet.(BinaryBuilder.supported_platforms()), " "))\"")
TRIPLETS="i686-linux-gnu x86_64-linux-gnu aarch64-linux-gnu arm-linux-gnueabihf powerpc64le-linux-gnu i686-linux-musl x86_64-linux-musl aarch64-linux-musl arm-linux-musleabihf x86_64-apple-darwin14 x86_64-unknown-freebsd11.1 i686-w64-mingw32 x86_64-w64-mingw32"

# These are the projects currently using BinaryBuilder; both GCC-expanded and non-GCC-expanded:
BB_PROJECTS="gmp mbedtls libssh2 mpfr curl libgit2 pcre libuv unwind osxunwind dsfmt objconv p7zip zlib suitesparse openlibm"
BB_GCC_EXPANDED_PROJECTS="openblas"
BB_CXX_EXPANDED_PROJECTS="llvm"

# If we've been given a project name, filter down to that one:
if [ -n "${1}" ]; then
    case "${BB_PROJECTS}" in
        *${1}*) BB_PROJECTS="${1}" ;;
        *) BB_PROJECTS="" ;;
    esac
    case "${BB_GCC_EXPANDED_PROJECTS}" in
        *${1}*) BB_GCC_EXPANDED_PROJECTS="${1}" ;;
        *) BB_GCC_EXPANDED_PROJECTS="" ;;
    esac
    case "${BB_CXX_EXPANDED_PROJECTS}" in
        *${1}*) BB_CXX_EXPANDED_PROJECTS="${1}" ;;
        *) BB_CXX_EXPANDED_PROJECTS="" ;;
    esac
fi

# Get "contrib/" directory path
CONTRIB_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# For each triplet and each project, download the BB tarball and save its hash:
for triplet in ${TRIPLETS}; do
	for proj in ${BB_PROJECTS}; do
		PROJ="$(echo ${proj} | tr [a-z] [A-Z])"
        make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet} distclean-${proj}
		make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet} install-${proj}
	done

    for proj in ${BB_GCC_EXPANDED_PROJECTS}; do
		PROJ="$(echo ${proj} | tr [a-z] [A-Z])"
        for libgfortran in libgfortran3 libgfortran4 libgfortran5; do
		    make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet}-${libgfortran} BB_TRIPLET_CXXABI=${triplet} distclean-${proj}
		    make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet}-${libgfortran} BB_TRIPLET_CXXABI=${triplet} install-${proj}
        done
    done

    for proj in ${BB_CXX_EXPANDED_PROJECTS}; do
		PROJ="$(echo ${proj} | tr [a-z] [A-Z])"
        for cxx in cxx03 cxx11; do
		    make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet}-${cxx} BB_TRIPLET_CXXABI=${triplet} distclean-${proj}
		    make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet}-${cxx} BB_TRIPLET_CXXABI=${triplet} install-${proj}
        done
    done
done
