#!/bin/sh
# FreeBSD CI Build Scripts
# The flow of a FreeBSD CI (https://freebsdci.julialang.org) build:
#
# 1. `compile`
# 2. `build-state`
# 3. `runtests`
# 4. `test-embedding`
#
# Detail of flow is controlled by the variable `factory`
# here.
# https://github.com/iblis17/julia-fbsd-buildbot/blob/master/master/master.cfg
#
# Usage: .freebsdci.sh <stage>

set -xe

build-state(){
    gmake build-stats
}

compile(){
    export MALLOC_CONF='junk:false'
    export VERBOSE=1
    export FORCE_ASSERTIONS=1
    export LLVM_ASSERTIONS=1

    gmake all -j $MAKE_JOBS_NUMBER
}

runtests(){
    export MALLOC_CONF='junk:false'
    export VERBOSE=1
    export FORCE_ASSERTIONS=1
    export LLVM_ASSERTIONS=1
    export JULIA_TEST_MAXRSS_MB=600
    export JULIA_CPU_CORES=$MAKE_JOBS_NUMBER

    ./usr/bin/julia test/runtests.jl all
    ./usr/bin/julia test/runtests.jl LibGit2/online Pkg/pkg Pkg3/pkg download
}

test-embedding(){
    export JULIA='../../julia'
    export BIN='../../tmp'

    mkdir -vp tmp
    gmake -C test embedding
}


if [ -z $1 ]
then
    exit 1
fi

$1
