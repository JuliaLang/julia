# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

using Base.Experimental: gc_compatible_pointer

donothing() = nothing
@noinline opaquecall() = (_dont_inline_; Int64(0))

function set1!(xs, T, x, pre_store)
    handle, ptr = gc_compatible_pointer(T(x))
    pre_store()
    GC.@preserve xs handle begin
        unsafe_store!(Ptr{Ptr{Cvoid}}(pointer(xs, 1)), ptr)
    end
    return Int64(123456789)
end
# Following examples check that:
# * An opaque call at `pre_store` should introduce a GC frame.
# * Only one allocation per call.

# CHECK-LABEL: @julia_set1_int_donothing
set1_int_donothing(xs, x) = set1!(xs, identity, x, donothing)
# CHECK: call {{.*}} @jl_box_int
# CHECK-NOT: %gcframe
# CHECK: ret i64 123456789

# CHECK-LABEL: @julia_set1_int_opaquecall
set1_int_opaquecall(xs, x) = set1!(xs, identity, x, opaquecall)
# CHECK: call {{.*}} @jl_box_int
# CHECK: %gcframe
# CHECK: call i64 @j_opaquecall
# CHECK: ret i64 123456789

# CHECK-LABEL: @julia_set1_someint_donothing
set1_someint_donothing(xs, x) = set1!(xs, Some, x, donothing)
# CHECK: call {{.*}} @jl_gc_pool_alloc
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK-NOT: %gcframe
# CHECK: ret i64 123456789

# CHECK-LABEL: @julia_set1_someint_opaquecall
set1_someint_opaquecall(xs, x) = set1!(xs, Some, x, opaquecall)
# CHECK: call {{.*}} @jl_gc_pool_alloc
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: %gcframe
# CHECK: call i64 @j_opaquecall
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: ret i64 123456789

# CHECK-LABEL: @julia_set1_refint_donothing
set1_refint_donothing(xs, x) = set1!(xs, Ref, x, donothing)
# CHECK: call {{.*}} @jl_gc_pool_alloc
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK-NOT: %gcframe
# CHECK: ret i64 123456789

# CHECK-LABEL: @julia_set1_refint_opaquecall
set1_refint_opaquecall(xs, x) = set1!(xs, Ref, x, opaquecall)
# CHECK: call {{.*}} @jl_gc_pool_alloc
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: %gcframe
# CHECK: call i64 @j_opaquecall
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: ret i64 123456789

emit(set1_int_donothing, Vector{Any}, Int)
emit(set1_int_opaquecall, Vector{Any}, Int)
emit(set1_someint_donothing, Vector{Any}, Int)
emit(set1_someint_opaquecall, Vector{Any}, Int)
emit(set1_refint_donothing, Vector{Any}, Int)
emit(set1_refint_opaquecall, Vector{Any}, Int)
