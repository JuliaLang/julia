#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Script to download newest version of cmake on linux (or mac)
# saves you the trouble of compiling it if you don't have root
set -e # stop on failure
cd "$(dirname "$0")"/.. # run in top-level directory

CMAKE_VERSION_MAJOR=3
CMAKE_VERSION_MINOR=7
CMAKE_VERSION_PATCH=1
CMAKE_VERSION_MAJMIN=$CMAKE_VERSION_MAJOR.$CMAKE_VERSION_MINOR
CMAKE_VERSION=$CMAKE_VERSION_MAJMIN.$CMAKE_VERSION_PATCH

PLATFORM="$(uname)-$(uname -m)"
FULLNAME=cmake-$CMAKE_VERSION-$PLATFORM
case $PLATFORM in
  Darwin-x86_64)
    CMAKE_EXTRACTED_PATH=$FULLNAME/CMake.app/Contents/bin/cmake;;
  Linux-x86_64)
    CMAKE_EXTRACTED_PATH=$FULLNAME/bin/cmake;;
  *)
    echo "This script only supports x86_64 Mac and Linux. For other platforms," >&2
    echo "get cmake from your package manager or compile it from source." >&2
    exit 1;;
esac

deps/tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
deps/tools/jlchecksum "$FULLNAME.tar.gz"
tar -xzf $FULLNAME.tar.gz
echo "CMAKE = $PWD/$CMAKE_EXTRACTED_PATH" >> Make.user
