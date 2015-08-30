#!/bin/bash
# This file is a part of Julia. License is MIT: http://julialang.org/license

set -e
mirror=http://mirrors.mit.edu/cygwin
platform=x86
packages="mintty cygwin coreutils libintl8 libiconv2 libgcc1"
local_path=./cygwin

# make place to put downloads
mkdir -p $local_path

# get setup.ini
curl $mirror/$platform/setup.bz2 -o $local_path/setup.ini.bz2
bunzip2 -f $local_path/setup.ini.bz2

# get and unpack packages
for package in $packages
do
    # determine the relative URL
    url=$(awk "BEGIN {RS = \"@ \"} ; \$1 == \"$package\" { print \$0 }" \
          $local_path/setup.ini | awk '$1 == "install:" { print $2 }' - | \
          head -1)

    # determine the local filename
    filename=${url##*/}

    # download the file
    curl $mirror/$url -o $local_path/$filename

    # unpack it
    tar xf $local_path/$filename --directory $local_path
done
