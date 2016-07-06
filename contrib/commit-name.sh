#!/bin/sh
# This file is a part of Julia. License is MIT: http://julialang.org/license

# Need to be run from a julia repo clone
# First argument (Optional) is a ref to the commit

gitref=${1:-HEAD}

last_tag=$(git describe --tags --abbrev=0 "$gitref")
ver=$(git show "$gitref:VERSION")
nb=$(git rev-list --count "$gitref" "^$last_tag")
echo "$ver+$nb"
