# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

## Notes:
# This script uses the `emit` function (defined llvmpasses.jl) to emit either
# optimized or unoptimized LLVM IR. Each function is emitted individually and
# `llvm-link` is used to create a single module that can be passed to opt.
# The order in which files are emitted and linked is important since `lit` will
# process the test cases in order.

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: check basic parameter names
function f1(a, b, c, d)
    return a + b + c + d
end

# COM: check basic parameter names + varargs
function f2(a, b, c, d, e...)
    return a + b + c + d + sum(e)
end

# COM: check basic parameter names + array allocation function name
function f3(a, b, c, d)
    return [a + b + c + d]
end

# COM: check basic parameter name + array allocation function name + array
function f4(n)
    return zeros(n)
end

mutable struct D
    i::Int64
end
struct C
    d::D
end
struct B
    c::C
end
struct A
    b::B
end

# COM: check getfield/setfield names
function f5(a)
    a.b.c.d.i = 0
    return a.b.c.d
end

struct H end
struct G
    h::Ref{H}
end
struct F
    g::Ref{G}
end
struct E
    f::Ref{F}
end

# COM: check gc lowering names
function f6(e)
    return e.f[].g[].h[]
end

# COM: check getfield for Tuples
function f7(a)
    return a[2]
end

# COM: check write barrier names
mutable struct Barrier
    b
end

# CHECK-LABEL: define {{(swiftcc )?}}double @julia_f1
# CHECK-SAME: double %"a::Float64"
# CHECK-SAME: double %"b::Float64"
# CHECK-SAME: double %"c::Float64"
# CHECK-SAME: double %"d::Float64"

# CHECK: fadd double
# CHECK-DAG: %"a::Float64"
# CHECK-DAG: %"b::Float64"
# CHECK-DAG: fadd double
# CHECK-DAG: %"c::Float64"
# CHECK-DAG: fadd double
# CHECK-DAG: %"d::Float64"
# CHECK: ret double
# CHECK: }

# CHECK-LABEL: define nonnull {} addrspace(10)* @jfptr_f1
# CHECK-SAME: %"function::Core.Function"
# CHECK-SAME: %"args::Any[]"
# CHECK-SAME: %"nargs::UInt32"
# CHECK: %"+Core.Float64
# CHECK: ret {} addrspace(10)*
# CHECK: }
emit(f1, Float64, Float64, Float64, Float64)

# CHECK: define {{(swiftcc )?}}double @julia_f2
# CHECK-SAME: double %"a::Float64"
# CHECK-SAME: double %"b::Float64"
# CHECK-SAME: double %"c::Float64"
# CHECK-SAME: double %"d::Float64"
# CHECK-SAME: double %"e[1]::Float64"
emit(f2, Float64, Float64, Float64, Float64, Float64)

# CHECK: define {{(swiftcc )?}}double @julia_f2
# CHECK-SAME: double %"a::Float64"
# CHECK-SAME: double %"b::Float64"
# CHECK-SAME: double %"c::Float64"
# CHECK-SAME: double %"d::Float64"
# CHECK-SAME: double %"e[1]::Float64"
# CHECK-SAME: double %"e[2]::Float64"
emit(f2, Float64, Float64, Float64, Float64, Float64, Float64)


# CHECK: define {{(swiftcc )?}}double @julia_f2
# CHECK-SAME: double %"a::Float64"
# CHECK-SAME: double %"b::Float64"
# CHECK-SAME: double %"c::Float64"
# CHECK-SAME: double %"d::Float64"
# CHECK-SAME: double %"e[1]::Float64"
# CHECK-SAME: double %"e[2]::Float64"
# CHECK-SAME: double %"e[3]::Float64"
emit(f2, Float64, Float64, Float64, Float64, Float64, Float64, Float64)

# CHECK: define {{(swiftcc )?}}nonnull {} addrspace(10)* @julia_f3
# CHECK-SAME: double %"a::Float64"
# CHECK-SAME: double %"b::Float64"
# CHECK-SAME: double %"c::Float64"
# CHECK-SAME: double %"d::Float64"
# CHECK: call nonnull {} addrspace(10)* {{.*}} @jlplt_ijl_alloc_array_1d
# CHECK-SAME: @"+Core.Array
emit(f3, Float64, Float64, Float64, Float64)

# CHECK: define {{(swiftcc )?}}nonnull {} addrspace(10)* @julia_f4
# CHECK-SAME: %"n::Int64"
# CHECK: call nonnull {} addrspace(10)* {{.*}} @jlplt_ijl_alloc_array_1d
# CHECK-SAME: @"+Core.Array
# CHECK: %.length_ptr
# CHECK: %.length
# CHECK: %.data
emit(f4, Int64)

# CHECK: define {{(swiftcc )?}}nonnull {} addrspace(10)* @julia_f5
# CHECK-SAME: %"a::A"
# CHECK: %"a::A.b_ptr.c_ptr.d
emit(f5, A)

# CHECK: define {{(swiftcc )?}}nonnull {} addrspace(10)* @julia_f6
# CHECK-SAME: %"e::E"
# CHECK: %jlcallframe
# CHECK: %gcframe
# CHECK: %frame.nroots
# CHECK: %frame.prev
# CHECK: %task.gcstack
# CHECK: %ptls_field
# CHECK: %ptls_load
# CHECK: %safepoint
# CHECK: %"e::E.f_ptr"
# CHECK: %"e::E.f"
# CHECK: %"e::E.f.tag_addr"
# CHECK: %"e::E.f.tag"
# CHECK: @"+Main.Base.RefValue
# CHECK: %gc_slot_addr_0
# CHECK: @"jl_sym#g
# CHECK: @"jl_sym#h
emit(f6, E)


# CHECK: define {{(swiftcc )?}}i64 @julia_f7
# CHECK-SAME: %"a::Tuple"
# CHECK: %"a::Tuple[2]_ptr.unbox
emit(f7,Tuple{Int,Int})

# CHECK: define {{(swiftcc )?}}nonnull {} addrspace(10)* @julia_Barrier
# CHECK-SAME: %"b::Int64"
# CHECK: %parent_bits
# CHECK: %parent_old_marked
# CHECK: %child_bit
# CHECK: %child_not_marked
emit(Barrier, Int64)
