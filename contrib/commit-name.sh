#!/bin/bash
# This file is a part of Julia. License is MIT: http://julialang.org/license

last_tag=$(git describe --tags --abbrev=0)
echo -n "0.4.0-dev+"
git rev-list ${1-HEAD} ^$last_tag | wc -l | sed -e 's/[^[:digit:]]//g'
