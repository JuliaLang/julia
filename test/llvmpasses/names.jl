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

# COM: check write barrier names and struct names
mutable struct Barrier
    b
end

# COM: check write barrier names
function f8(b,y)
    b.b = y
    return b
end

struct Named
    x::Int
end

function fmemory(nel)
    return Memory{Int64}(undef,nel)
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

# CHECK-LABEL: define nonnull ptr @jfptr_f1
# CHECK-SAME: %"function::Core.Function"
# CHECK-SAME: %"args::Any[]"
# CHECK-SAME: %"nargs::UInt32"
# CHECK: %"+Core.Float64
# CHECK: ret ptr
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

# CHECK: define {{(swiftcc )?}}nonnull ptr @julia_f5
# CHECK-SAME: %"a::A"
# CHECK: %"a::A.d
# COM: this text check relies on our LLVM code emission being relatively poor, which is not always the case
emit(f5, A)

# CHECK: define {{(swiftcc )?}}nonnull ptr @julia_f6
# CHECK-SAME: %"e::E"
# CHECK: %jlcallframe
# CHECK: %gcframe
# CHECK: %frame.prev
# CHECK: %task.gcstack
# CHECK: %ptls_field
# CHECK: %ptls_load
# CHECK: %safepoint
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
emit(f7, Tuple{Int,Int})

# CHECK: define {{(swiftcc )?}}nonnull ptr @julia_f8
# CHECK-SAME: %"y::Int64"
# CHECK: %parent_bits
# CHECK: %parent_old_marked
# CHECK: %child_bit
# CHECK: %child_not_marked
emit(f8, Barrier, Int)

# CHECK: define {{(swiftcc )?}}nonnull ptr @julia_Barrier
# CHECK-SAME: %"b::Named"
# CHECK: %"new::Barrier"
# CHECK: %"box::Named"
emit(Barrier, Named)

# CHECK: define {{(swiftcc )?}}nonnull ptr @julia_fmemory
# CHECK-SAME: %"nel::Int64"
# CHECK: %"Memory{Int64}[]"
emit(fmemory, Int64)
