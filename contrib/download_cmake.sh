#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Script to download newest version of cmake on linux (or mac)
# saves you the trouble of compiling it if you don't have root
set -e # stop on failure
mkdir -p "$(dirname "$0")"/../deps/scratch
cd "$(dirname "$0")"/../deps/scratch

CMAKE_VERSION_MAJOR=3
CMAKE_VERSION_MINOR=31
CMAKE_VERSION_PATCH=6
CMAKE_VERSION_MAJMIN=$CMAKE_VERSION_MAJOR.$CMAKE_VERSION_MINOR
CMAKE_VERSION=$CMAKE_VERSION_MAJMIN.$CMAKE_VERSION_PATCH

# listed at https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/cmake-$CMAKE_VERSION-SHA-256.txt
# for the files cmake-$CMAKE_VERSION-macos-universal.tar.gz
# cmake-$CMAKE_VERSION-Linux-x86_64.tar.gz and cmake-$CMAKE_VERSION-Linux-aarch64.tar.gz
CMAKE_SHA256_DARWIN=330b9514f5112e5ed4fb08b8b05803b776fd9b539a6ae12927d14dcc0ee2ba8d
CMAKE_SHA256_LINUX_X86_64=5a1133ff103c71eb5120e2cc3de922733e7d8a26a98ae716397e8676adb367bf
CMAKE_SHA256_LINUX_AARCH64=b4cc788d63112b2749b40627e719eb5d3b8ed8f00c36d77189f4019cfe64bc9e

PLATFORM="$(uname)-$(uname -m)"
case $PLATFORM in
  Darwin-*)
    FULLNAME=cmake-$CMAKE_VERSION-macos-universal
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_DARWIN  $FULLNAME.tar.gz" | shasum -a 256 -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/CMake.app/Contents/bin/cmake;;
  Linux-x86_64)
    FULLNAME=cmake-$CMAKE_VERSION-linux-x86_64
    ../tools/jldownload https://cmake.org/files/v$CMAKE_VERSION_MAJMIN/$FULLNAME.tar.gz
    echo "$CMAKE_SHA256_LINUX_X86_64  $FULLNAME.tar.gz" | sha256sum -c -
    CMAKE_EXTRACTED_PATH=$FULLNAME/bin/cmake;;
  Linux-aarch64)
    FULLNAME=cmake-$CMAKE_VERSION-linux-aarch64
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
