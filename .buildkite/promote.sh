#!/usr/bin/env bash

set -eux

if [[ "$IS_MASTER" != "true" ]]; then
  echo "This step should not be running on pull requests, or on branches other than master"
  exit 1
fi

package="$(buildkite-agent meta-data get "package-$PROMOTE_OS-$PROMOTE_ARCH")"
buildkite-agent artifact download "$package" .
aws s3 cp "$package" "s3://$STAGING_BUCKET/bin/$(date +%s)-$RANDOM.$PKG_EXT" \
    --metadata "os=$PROMOTE_OS,arch=$PROMOTE_ARCH,version=$VERSION,filename=$package"
