#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Script to download newest version of cmake on linux (or mac)
# saves you the trouble of compiling it if you don't have root
set -e # stop on failure
mkdir -p "$(dirname "$0")"/../deps/scratch
cd "$(dirname "$0")"/../deps/scratch

CMAKE_VERSION_MAJOR=3
CMAKE_VERSION_MINOR=19
CMAKE_VERSION_PATCH=2
CMAKE_VERSION_MAJMIN=$CMAKE_VERSION_MAJOR.$CMAKE_VERSION_MINOR
CMAKE_VERSION=$CMAKE_VERSION_MAJMIN.$CMAKE_VERSION_PATCH

# listed at https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/cmake-$CMAKE_VERSION-SHA-256.txt
# for the files cmake-$CMAKE_VERSION-Darwin-x86_64.tar.gz
# and cmake-$CMAKE_VERSION-Linux-x86_64.tar.gz
CMAKE_SHA256_DARWIN=50afa2cb66bea6a0314ef28034f3ff1647325e30cf5940f97906a56fd9640bd8
CMAKE_SHA256_LINUX=4d8a6d852c530f263b22479aad196416bb4406447e918bd9759c6593b7f5f3f9

PLATFORM="$(uname)-$(uname -m)"
FULLNAME=cmake-$CMAKE_VERSION-$PLATFORM
case $PLATFORM in
  Darwin-x86_64)
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_DARWIN  $FULLNAME.tar.gz" | shasum -a 256 -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/CMake.app/Contents/bin/cmake;;
  Linux-x86_64)
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_LINUX  $FULLNAME.tar.gz" | sha256sum -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/bin/cmake;;
  *)
    echo "This script only supports x86_64 Mac and Linux. For other platforms," >&2
    echo "get cmake from your package manager or compile it from source." >&2
    exit 1;;
esac

tar -xzf $FULLNAME.tar.gz
echo "CMAKE = $PWD/$CMAKE_EXTRACTED_PATH" >> ../../Make.user
