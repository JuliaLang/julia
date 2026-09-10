# This file is a part of Julia. License is MIT: https://julialang.org/license
# RUN: julia --startup-file=no --check-bounds=yes %s %t -O
# RUN: cat %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# Test for GenericMemoryRef address space bug
# Issue: stores incorrectly use addrspace(10) instead of addrspace(11)
# in bounds checking code, causing LLVM assertion failures
#
# The `boundscheck` argument of `memoryrefnew` is passed explicitly here.
# Writing `x[i]` would not do: `Base.getindex` validates the index with its own
# `@boundscheck` and then passes `false`, so the intrinsic emits no check of its
# own and this bounds-checking path would not be reached.
function bf(i, x)
    ref = Core.memoryrefnew(getfield(x, :ref), i, true)
    v = Core.memoryrefget(ref, :not_atomic, false)
    Core.memoryrefset!(ref, v * v, :not_atomic, false)
    nothing
end

# CHECK-LABEL: @julia_bf
# CHECK: oob:
# CHECK: store ptr {{.*}}, ptr %"box::GenericMemoryRef"
# CHECK-NOT: store {{.*}} addrspace(10) {{.*}}GenericMemoryRef
# CHECK: call void @ijl_bounds_error_int

emit(bf, Int, Vector{Float64})
