#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Script to download newest version of cmake on linux (or mac)
# saves you the trouble of compiling it if you don't have root
set -e # stop on failure
mkdir -p "$(dirname "$0")"/../deps/scratch
cd "$(dirname "$0")"/../deps/scratch

CMAKE_VERSION_MAJOR=3
CMAKE_VERSION_MINOR=19
CMAKE_VERSION_PATCH=3
CMAKE_VERSION_MAJMIN=$CMAKE_VERSION_MAJOR.$CMAKE_VERSION_MINOR
CMAKE_VERSION=$CMAKE_VERSION_MAJMIN.$CMAKE_VERSION_PATCH

# listed at https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/cmake-$CMAKE_VERSION-SHA-256.txt
# for the files cmake-$CMAKE_VERSION-macos-universal.tar.gz
# cmake-$CMAKE_VERSION-Linux-x86_64.tar.gz and cmake-$CMAKE_VERSION-Linux-aarch64.tar.gz
CMAKE_SHA256_DARWIN=a6b79ad05f89241a05797510e650354d74ff72cc988981cdd1eb2b3b2bda66ac
CMAKE_SHA256_LINUX_X86_64=c18b65697e9679e5c88dccede08c323cd3d3730648e59048047bba82097e0ffc
CMAKE_SHA256_LINUX_AARCH64=66e507c97ffb586d7ca6567890808b792c8eb004b645706df6fbf27826a395a2

PLATFORM="$(uname)-$(uname -m)"
case $PLATFORM in
  Darwin-*)
    FULLNAME=cmake-$CMAKE_VERSION-macos-universal
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_DARWIN  $FULLNAME.tar.gz" | shasum -a 256 -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/CMake.app/Contents/bin/cmake;;
  Linux-x86_64)
    FULLNAME=cmake-$CMAKE_VERSION-$PLATFORM
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_LINUX_X86_64  $FULLNAME.tar.gz" | sha256sum -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/bin/cmake;;
  Linux-aarch64)
    FULLNAME=cmake-$CMAKE_VERSION-$PLATFORM
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_LINUX_AARCH64  $FULLNAME.tar.gz" | sha256sum -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/bin/cmake;;
  *)
    echo "This script only supports Mac and Linux, both for x86_64 and aarch64." >&2
    echo "For other platforms, get cmake from your package manager or compile it from source." >&2
    exit 1;;
esac

tar -xzf $FULLNAME.tar.gz
echo "CMAKE = $PWD/$CMAKE_EXTRACTED_PATH" >> ../../Make.user
