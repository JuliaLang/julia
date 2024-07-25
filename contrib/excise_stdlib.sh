#!/usr/bin/env bash

# Uses https://github.com/newren/git-filter-repo
# Recommended use of `Github cli`

set -e
set -f
set -x

if [ -z "$*" ]; then echo "Expected name of stdlib"; fi

STDLIB=$1
WORKDIR=$(mktemp -d)

echo "Excising stdlib $STDLIB; workdir $WORKDIR"
pushd $WORKDIR
git clone https://github.com/JuliaLang/julia $STDLIB
pushd $STDLIB

echo "Filtering repo"
git filter-repo --subdirectory-filter stdlib/$STDLIB --path LICENSE.md \
    --message-callback 'return re.sub(b"(\W)(#\d+)", lambda m: m.group(1) + b"JuliaLang/julia" + m.group(2), message)'


echo "Deleting branches"
git branch -l | grep -v release- | grep -v master | xargs git branch -v -D

popd
popd
echo "Done! Inspect the result and push it!"
echo """
      cd $WORKDIR/$STDLIB
      gh repo create JuliaLang/$STDLIB.jl --push --source=. --public
      git push --all
      git push --tags"""

echo """
     Remember to:
     1. Add a README.md
     2. Setup GHA or similar for CI
     """
