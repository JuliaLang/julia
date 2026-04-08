# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O2 --check-bounds=no %s %t -O && llvm-link -S %t/* | FileCheck %s

## Test that per-type TBAA enables concrete optimizations:
## - Redundant load elimination across stores to different types
## - Loop-invariant load hoisting when only a different type is stored

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# Test 1: Redundant load elimination
# Load b.y, store to a.x, load b.y again.
# Since Foo and Bar have different TBAA, the second load should be eliminated.
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

# Test 2: Loop-invariant hoisting
# b.y is loaded inside a loop that only stores to a.x.
# Since Foo and Bar have different TBAA, b.y should be hoisted out of the loop.
mutable struct LoopFoo; x::Int; end
mutable struct LoopBar; y::Int; end

# CHECK-LABEL: @julia_loop_hoist
# CHECK: load i64
# CHECK-NOT: load i64
# CHECK: ret
function loop_hoist(a::LoopFoo, b::LoopBar, n::Int)
    s = 0
    for i in 1:n
        s += b.y
        a.x = s
    end
    return s
end

# Test 3: Array element type disambiguation
# Store to Int64 array, load from Float64 array.
# These should get different TBAA tags so the load is independent of the store.
# CHECK-LABEL: @julia_array_eltype_noalias
# CHECK: store i64
# CHECK: load double
# CHECK: ret
function array_eltype_noalias(a::Vector{Int64}, b::Vector{Float64}, val::Int64)
    a[1] = val
    return b[1]
end

emit(redundant_load_elim, RLEFoo, RLEBar)
emit(loop_hoist, LoopFoo, LoopBar, Int)
emit(array_eltype_noalias, Vector{Int64}, Vector{Float64}, Int64)
