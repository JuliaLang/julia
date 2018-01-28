#!/bin/sh

set -xe

export MALLOC_CONF='junk:false'
export VERBOSE=1
export FORCE_ASSERTIONS=1
export LLVM_ASSERTIONS=1
export JULIA_TEST_MAXRSS_MB=600
export JULIA_CPU_CORES=$MAKE_JOBS_NUMBER

gmake testall \
      test-download \
      test-pkg \
      test-libgit2-online
