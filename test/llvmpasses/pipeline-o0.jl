# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O0 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O1 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O2 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s
# RUN: julia --startup-file=no -O3 --check-bounds=yes %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_simple
# CHECK-NOT: julia.get_pgcstack
# CHECK-NOT: julia.gc_alloc_obj
# CHECK: ijl_gc_small_alloc
# COM: we want something vaguely along the lines of asm load from the fs register -> allocate bytes
function simple()
    Ref(0)
end

# CHECK-LABEL: @julia_buildarray
# CHECK-NOT: julia.write_barrier
# CHECK: gc_queue_root
function buildarray()
    out = []
    for i in 1:100
        push!(out, Ref(0))
    end
    out
end

emit(simple)
emit(buildarray)
