#!/bin/bash

if [[ "${BUILDKITE_PULL_REQUEST:?}" != "false" ]]; then
    git log -n 1
    echo ""
    echo "This is a pull request build."
    echo "Pull request: ${BUILDKITE_PULL_REQUEST:?}"
    echo "Fetching the pull request merge commit..."
    sleep 0.01 # Make sure GitHub has enough time to create the merge commit.
    git fetch origin +refs/pull/${BUILDKITE_PULL_REQUEST:?}/merge
    echo "Checking out the pull request merge commit..."
    git checkout --force FETCH_HEAD
else
    echo "This is not a pull request build."
fi

git log -n 1
echo ""
