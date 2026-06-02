# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O2 --check-bounds=no %s %t -O && llvm-link -S %t/* | FileCheck %s

## Test that per-type TBAA enables concrete optimizations:
## - Redundant load elimination across stores to different types
## - Loop-invariant load hoisting when only a different type is stored
## - Struct-path TBAA: different fields of the same struct don't alias

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# Test 1: Redundant load elimination across different struct types
mutable struct RLEFoo; x::Int; end
mutable struct RLEBar; y::Int; end

# CHECK-LABEL: @julia_redundant_load_elim
# CHECK: load i64
# CHECK: store i64
# CHECK-NOT: load i64
# CHECK: ret
function redundant_load_elim(a::RLEFoo, b::RLEBar)
    y1 = b.y
    a.x = y1 + 1
    y2 = b.y
    return y1 + y2
end

# Test 2: Loop-invariant hoisting across different struct types
mutable struct LoopFoo; x::Int; end
mutable struct LoopBar; y::Int; end

# CHECK-LABEL: @julia_loop_hoist_types
# CHECK: load i64
# CHECK-NOT: load i64
# CHECK: ret
function loop_hoist_types(a::LoopFoo, b::LoopBar, n::Int)
    s = 0
    for i in 1:n
        s += b.y
        a.x = s
    end
    return s
end

# Test 3: Struct-path TBAA — different fields of the SAME struct type
# p and q might be the same object, but .x (offset 0) and .y (offset 8)
# are at different offsets, so struct-path TBAA proves NoAlias.
mutable struct SPPoint; x::Float64; y::Float64; end

# CHECK-LABEL: @julia_loop_hoist_fields
# CHECK: load double
# CHECK-NOT: load double
# CHECK: ret
function loop_hoist_fields(p::SPPoint, q::SPPoint, n::Int)
    s = 0.0
    for i in 1:n
        s += q.y
        p.x = s
    end
    return s
end

# Test 4: Array element type disambiguation
# CHECK-LABEL: @julia_array_eltype_noalias
# CHECK: store i64
# CHECK: load double
# CHECK: ret
function array_eltype_noalias(a::Vector{Int64}, b::Vector{Float64}, val::Int64)
    a[1] = val
    return b[1]
end

emit(redundant_load_elim, RLEFoo, RLEBar)
emit(loop_hoist_types, LoopFoo, LoopBar, Int)
emit(loop_hoist_fields, SPPoint, SPPoint, Int)
emit(array_eltype_noalias, Vector{Int64}, Vector{Float64}, Int64)
