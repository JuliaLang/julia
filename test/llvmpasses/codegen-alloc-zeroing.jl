# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for GC pointer zeroing operand bundles on allocation calls
# These bundles inform late-gc-lowering about which fields need to be zeroed

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: Test 1: Mutable struct with pointer fields should emit julia.gc_alloc_ptr_offsets bundle
mutable struct TestStructPtrs
    a::Any      # GC pointer at offset 0
    b::Int64    # Non-pointer at offset 8
    c::Any      # GC pointer at offset 16
end

function make_struct_ptrs(x, y, z)
    TestStructPtrs(x, y, z)
end

# CHECK: define {{.*}} @julia_make_struct_ptrs
# CHECK: julia.gc_alloc_obj
# CHECK-SAME: [ "julia.gc_alloc_ptr_offsets"(i64 0, i64 16) ]
emit(make_struct_ptrs, Any, Int64, Any)

# COM: Test 2: Struct with only non-pointer fields should NOT have the bundle
mutable struct TestStructNoPtrs
    a::Int64
    b::Int64
end

function make_struct_no_ptrs(x, y)
    TestStructNoPtrs(x, y)
end

# CHECK: define {{.*}} @julia_make_struct_no_ptrs
# CHECK: julia.gc_alloc_obj
# CHECK-NOT: julia.gc_alloc_ptr_offsets
# CHECK: ret
emit(make_struct_no_ptrs, Int64, Int64)

# COM: Test 3: Memory{Any} (constant length) should emit julia.gc_alloc_zeroinit bundle
function make_memory_any()
    Memory{Any}(undef, 4)
end

# CHECK: define {{.*}} @julia_make_memory_any
# CHECK: julia.gc_alloc_obj
# CHECK-SAME: [ "julia.gc_alloc_zeroinit"(i64 16, i64 32) ]
emit(make_memory_any)

# COM: Test 4: Memory{Int64} (non-boxed) should NOT have zeroinit bundle
function make_memory_int()
    Memory{Int64}(undef, 4)
end

# CHECK: define {{.*}} @julia_make_memory_int
# CHECK-NOT: julia.gc_alloc_zeroinit
# CHECK: ret
emit(make_memory_int)

# COM: Test 5: Single pointer field struct
mutable struct SinglePtr
    x::Any
end

function make_single_ptr(x)
    SinglePtr(x)
end

# CHECK: define {{.*}} @julia_make_single_ptr
# CHECK: julia.gc_alloc_obj
# CHECK-SAME: [ "julia.gc_alloc_ptr_offsets"(i64 0) ]
emit(make_single_ptr, Any)

# COM: Test 6: Variable-length Memory{Any} uses zeroinit_indirect bundle
function make_memory_any_dynamic(n::Int)
    Memory{Any}(undef, n)
end

# CHECK: define {{.*}} @julia_make_memory_any_dynamic
# CHECK: jl_alloc_genericmemory_unchecked
# CHECK-SAME: [ "julia.gc_alloc_zeroinit_indirect"(i64 8,
emit(make_memory_any_dynamic, Int)

# COM: Test 7: Variable-length Memory{Int64} should NOT have zeroinit bundle
function make_memory_int_dynamic(n::Int)
    Memory{Int64}(undef, n)
end

# CHECK: define {{.*}} @julia_make_memory_int_dynamic
# CHECK: jl_alloc_genericmemory_unchecked
# CHECK-NOT: julia.gc_alloc_zeroinit_indirect
# CHECK: ret
emit(make_memory_int_dynamic, Int)

# COM: Test 8: SimpleVector (svec) should emit julia.gc_alloc_zeroinit bundle
function make_svec(a, b, c)
    Core.svec(a, b, c)
end

# CHECK: define {{.*}} @{{(julia_make_svec|japi1_make_svec)}}
# CHECK: julia.gc_alloc_obj
# CHECK-SAME: [ "julia.gc_alloc_zeroinit"(i64 8, i64 24) ]
emit(make_svec, Any, Any, Any)
