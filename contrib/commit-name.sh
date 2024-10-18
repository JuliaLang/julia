#!/bin/sh
# This file is a part of Julia. License is MIT: https://julialang.org/license

# Needs to be run from a julia repo clone
# First argument (optional) is a ref to the commit

gitref=${1:-HEAD}

ver=$(git show "$gitref:VERSION")
major=$(echo $ver | cut -f 1 -d .)
minor=$(echo $ver | cut -f 2 -d .)

if [ $major = 0 -a $minor -lt 5 ]; then
    # use tag based build number prior to 0.5.0-
    last_tag=$(git describe --tags --abbrev=0 "$gitref")
    nb=$(git rev-list --count "$gitref" "^$last_tag")
    if [ $nb = 0 ]; then
        echo $ver
    else
        echo "$ver+$nb"
    fi
else
    topdir=$(git rev-parse --show-toplevel)
    verchanged=$(git blame -L ,1 -sl $gitref -- "$topdir/VERSION" | cut -f 1 -d " ")
    nb=$(git rev-list --count "$gitref" "^$verchanged")
    pre=$(echo $ver | cut -s -f 2 -d "-")
    if [ $ver = "0.5.0-dev" ]; then
        # bump to 0.5.0-dev was one commit after tag during 0.5.0-dev
        nb=$(expr $nb + 1)
    elif [ $ver = "0.5.0-pre" ]; then
        # bump to 0.5.0-pre was 5578 commits after tag
        nb=$(expr $nb + 5578)
    fi
    if [ -n "$pre" ]; then
        if [ $major = 0 -a $minor -le 5 ]; then
            echo "$ver+$nb"
        else
            echo "$ver.$nb"
        fi
    else
        echo $ver
    fi
fi
