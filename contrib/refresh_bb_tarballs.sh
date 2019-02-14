#!/bin/sh

# Get this list via:
#    using BinaryBuilder
#    print("TRIPLETS=\"$(join(triplet.(BinaryBuilder.supported_platforms()), " "))\"")
TRIPLETS="i686-linux-gnu x86_64-linux-gnu aarch64-linux-gnu arm-linux-gnueabihf powerpc64le-linux-gnu i686-linux-musl x86_64-linux-musl aarch64-linux-musl arm-linux-musleabihf x86_64-apple-darwin14 x86_64-unknown-freebsd11.1 i686-w64-mingw32 x86_64-w64-mingw32"

# These are the projects currently using BinaryBuilder; both GCC-expanded and non-GCC-expanded:
BB_PROJECTS="llvm"
BB_GCC_EXPANDED_PROJECTS="openblas suitesparse openlibm"

# Get "contrib/" directory path
CONTRIB_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# For each triplet and each project, download the BB tarball and save its hash:
for triplet in ${TRIPLETS}; do
	for proj in ${BB_PROJECTS}; do
		PROJ="$(echo ${proj} | tr [a-z] [A-Z])"
        make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet} distclean-${proj}
		make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet} compile-${proj}
	done

    for proj in ${BB_GCC_EXPANDED_PROJECTS}; do
		PROJ="$(echo ${proj} | tr [a-z] [A-Z])"
        for gcc in gcc4 gcc7 gcc8; do
		    make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet}-${gcc} distclean-${proj}
		    make -C "${CONTRIB_DIR}/../deps" USE_BINARYBUILDER_${PROJ}=1 ${PROJ}_BB_TRIPLET=${triplet}-${gcc} compile-${proj}
        done
    done
done
