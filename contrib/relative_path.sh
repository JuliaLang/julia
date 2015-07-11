#!/usr/bin/env bash
# This file is a part of Julia. License is MIT: http://julialang.org/license

# both $1 and $2 are absolute paths beginning with /
# returns relative path to $2/$target from $1/$source

# Source copied from http://stackoverflow.com/a/12498485/230778

source=$1
target=$2

if [[ ! "$source" == /* ]] || [[ ! "$target" == /* ]]; then
    echo "ERROR: paths must be absolute paths, they must start with a forward slash!"
    exit 1
fi

common_part=$source # for now
result="" # for now

while [[ "${target#$common_part}" == "${target}" ]]; do
    # no match, means that candidate common part is not correct
    # go up one level (reduce common part)
    common_part="$(dirname $common_part)"
    # and record that we went back, with correct / handling
    if [[ -z $result ]]; then
        result=".."
    else
        result="../$result"
    fi
done

if [[ $common_part == "/" ]]; then
    # special case for root (no common path)
    result="$result/"
fi

# since we now have identified the common part,
# compute the non-common part
forward_part="${target#$common_part}"

# and now stick all parts together
if [[ -n $result ]] && [[ -n $forward_part ]]; then
    result="$result$forward_part"
elif [[ -n $forward_part ]]; then
    # extra slash removal
    result="${forward_part:1}"
fi

echo $result
