#!/bin/sh

set -xe

export MALLOC_CONF='junk:false'
export VERBOSE=1
export FORCE_ASSERTIONS=1
export LLVM_ASSERTIONS=1

gmake all -j $MAKE_JOBS_NUMBER
