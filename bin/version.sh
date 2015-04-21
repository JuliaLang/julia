#!/bin/bash
last_tag=$(git describe --tags --abbrev=0)
git rev-list $1 ^$last_tag | wc -l | sed -e 's/[^[:digit:]]//g'
