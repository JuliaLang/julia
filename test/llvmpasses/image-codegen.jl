# This file is a part of Julia. License is MIT: https://julialang.org/license
# RUN: export JULIA_LLVM_ARGS="--print-before=loop-vectorize --print-module-scope"
# RUN: rm -rf %t
# RUN: mkdir %t
# RUN: julia --image-codegen --startup-file=no %s 2> %t/output.txt
# RUN: FileCheck %s < %t/output.txt

# COM: checks that global variables compiled in imaging codegen
# COM: are marked as external and not internal
# COM: Also makes sure that --imaging-codegen doesn't crash

# CHECK: *** IR Dump Before
# CHECK-NOT: internal global
# CHECK-NOT: private global
# CHECK: jl_global
# COM: we emit both declarations and definitions, so we may see either style in the IR
# CHECK-SAME: = {{(external )?}}
# CHECK-NOT: internal global
# CHECK-NOT: private global

f() = "abcd"
f()
