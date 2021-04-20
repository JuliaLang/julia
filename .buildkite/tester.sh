#!/usr/bin/env bash

set -eux

export JULIA_TEST_NUM_CORES="$NTHREADS"
case "$ARCH" in
  armv7l)
    export JULIA_TEST_MAXRSS_MB=900
    ;;
esac

package="$(buildkite-agent meta-data get "package-$OS-$ARCH")"
buildkite-agent artifact download "$package" .

case "$PKG_EXT" in
  tar.gz)
    tar -zxvf "$package"
    JULIA="$(pwd)/julia-$SHORT_COMMIT/bin/julia"
    ;;
  dmg)
    hdiutil mount "$package" -mountpoint dmg_mount
    cp -a "dmg_mount/Julia-$VERSION.app/Contents/Resources/julia" .
    hdiutil detach dmg_mount
    JULIA="$(pwd)/julia/bin/julia"
    ;;
  exe)
    ;;
esac

code='
  include(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "test", "choosetests.jl"))
  Base.runtests(
    append!(choosetests()[1], ["LibGit2/online", "download"]);
    ncores=min(Sys.CPU_THREADS, 8, parse(Int, ENV["NTHREADS"])),
  )'
# "$JULIA" -e "$code"

# TODO: rr is not working.
# if [[ "$OS" == "linux" ]] && [[ "$ARCH" == "x86_64" || "$ARCH" == "i686" ]]; then
#   "$JULIA" "$(dirname $0)/rr_capture.jl" "$BUILDKITE_BUILD_ID" "$SHORT_COMMIT" "$JULIA" -e "$code"
# elif [[ "$OS" == "winnt" ]]; then
#   "$JULIA" "$(dirname $0)/autodump.jl" "$BUILDKITE_BUILD_ID" "$SHORT_COMMIT" "$JULIA" -e "$code"
# else
#   "$JULIA" -e "$code"
# fi
