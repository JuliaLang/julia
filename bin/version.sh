#!/bin/bash
last_tag=$(git describe --tags --abbrev=0)
ver=$(cat VERSION)
nb=$(git rev-list $1 ^$last_tag | wc -l | sed -e 's/[^[:digit:]]//g')
echo "$ver+$nb"
