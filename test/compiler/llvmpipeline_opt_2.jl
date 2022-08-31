# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using InteractiveUtils

# These tests act as a simple guard to ensure we don't break our pass pipeline
# They are NOT a promise of any kind of performance for any particular syntax
# Tests may be marked as broken at any time depending on the PR

# The file is organized into a few basic patterns of optimizations we
# want to occur, namely:
# * bounds check elimination
# * vectorization
# * memset idiom recognition
# * hoisting/induction/unrolling optimizations
# * allocation elision and stack allocation
# Also we want to be >= O1
include("./llvmpipeline_opt_1.jl")

# Test against offset arrays too
isdefined(Main, :OffsetArrays) || @eval Main include("../testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

if check_bounds != bc_off

# Test that various basic array patterns don't check bounds even with boundschecks
@testset "$prefix boundscheck elimination" begin

    # Ensure safe iteration over one array is not boundschecked
    function iterate_read(arr)
        total = zero(eltype(arr))
        for i in eachindex(arr)
            total += arr[i]
        end
        total
    end

    function iterate_write(arr, out)
        for i in eachindex(arr, out)
            out[i] = arr[i]
        end
    end

    function iterate_write!(arr)
        for i in eachindex(arr)
            arr[i] *= 2
        end
    end

    for tp in IntTypes
        # Controlled by LICM, IRCE, GVN
        check_llvm([("bounds_error", false)], iterate_read, Tuple{Vector{tp}})
        check_llvm([("bounds_error", false)], iterate_write, Tuple{Vector{tp}, Vector{tp}})
        check_llvm([("bounds_error", false)], iterate_write!, Tuple{Vector{tp}})
    end

    # The bounds error isn't full eliminated here, although iterate_read does vectorize
    @test occursin("boundserror", get_llvm_unopt(iterate_read, Tuple{OffsetArray{Int, 1, Vector{Int}}}))
    @test occursin("boundserror", get_llvm_unopt(iterate_write!, Tuple{OffsetArray{Int, 1, Vector{Int}}}))

    @test_broken !occursin("boundserror", get_llvm(iterate_read, Tuple{OffsetArray{Int, 1, Vector{Int}}}))
    @test_broken !occursin("boundserror", get_llvm(iterate_write!, Tuple{OffsetArray{Int, 1, Vector{Int}}}))

    # Ensure safe iteration over multiple arrays is not boundschecked
    function multiiterate_read(arr1, arr2)
        total = zero(eltype(arr1))
        for i in eachindex(arr1, arr2)
            total += arr1[i]
            total += arr2[i]
        end
        total
    end

    function multiiterate_write(arr1, arr2, arr3)
        for i in eachindex(arr1, arr2, arr3)
            arr3[i] += arr1[i]
            arr3[i] += arr2[i]
        end
    end

    function multiiterate_write!(arr1, arr2)
        for i in eachindex(arr1, arr2)
            arr1[i] += arr2[i]
        end
    end

    for tp in IntTypes
        # Controlled by LICM, IRCE, GVN
        check_llvm([("bounds_error", false)], multiiterate_read, Tuple{Vector{tp}, Vector{tp}})
        check_llvm([("bounds_error", false)], multiiterate_write, Tuple{Vector{tp}, Vector{tp}, Vector{tp}})
        check_llvm([("bounds_error", false)], multiiterate_write!, Tuple{Vector{tp}, Vector{tp}})
    end
end

end

# Test that various basic patterns vectorize
@testset "$prefix vectorization" begin

    # Ensure safe iteration over one array is vectorized
    function iterate_read(arr)
        total = zero(eltype(arr))
        for i in eachindex(arr)
            total += arr[i]
        end
        total
    end

    function iterate_write(arr, out)
        for i in eachindex(arr, out)
            out[i] = arr[i]
        end
    end

    function iterate_write!(arr)
        for i in eachindex(arr)
            arr[i] *= 2
        end
    end

    for tp in IntTypes
        # Controlled by LICM, IRCE, GVN, LoopVectorize, SLPVectorizer
        check_llvm([("vector.body", true)], iterate_read, Tuple{Vector{tp}})
        check_llvm([("vector.body", true)], iterate_write, Tuple{Vector{tp}, Vector{tp}})
        if tp == Int32 || tp == UInt32
            # Only Int64/UInt64 seem to vectorize this
            @test !occursin("vector.body", get_llvm_unopt(iterate_write!, Tuple{Vector{tp}}))
            @test_broken occursin("vector.body", get_llvm(iterate_write!, Tuple{Vector{tp}}))
        else
            check_llvm([("vector.body", true)], iterate_write!, Tuple{Vector{tp}})
        end
    end

    @test occursin("vector.body", get_llvm(iterate_read, Tuple{OffsetArray{Int64, 1, Vector{Int64}}}))
    if check_bounds == bc_off
        @test occursin("vector.body", get_llvm(iterate_write!, Tuple{OffsetArray{Int64, 1, Vector{Int64}}}))
    else
        @test_broken occursin("vector.body", get_llvm(iterate_write!, Tuple{OffsetArray{Int64, 1, Vector{Int64}}}))
    end

    # Ensure safe iteration over multiple arrays is vectorized
    function multiiterate_read(arr1, arr2)
        total = zero(eltype(arr1))
        for i in eachindex(arr1, arr2)
            total += arr1[i]
            total += arr2[i]
        end
        total
    end

    function multiiterate_write(arr1, arr2, arr3)
        for i in eachindex(arr1, arr2, arr3)
            arr3[i] += arr1[i]
            arr3[i] += arr2[i]
        end
    end

    function multiiterate_write!(arr1, arr2)
        for i in eachindex(arr1, arr2)
            arr1[i] += arr2[i]
        end
    end

    for tp in IntTypes
        check_llvm([("vector.body", true)], multiiterate_read, Tuple{Vector{tp}, Vector{tp}})
        check_llvm([("vector.body", true)], multiiterate_write, Tuple{Vector{tp}, Vector{tp}, Vector{tp}})
        check_llvm([("vector.body", true)], multiiterate_write!, Tuple{Vector{tp}, Vector{tp}})
    end
end

# Check that initializing arrays with 0 delegates to memset instead of a loop
# Relies on LICM, loop-idiom, loop-deletion
@testset "$prefix zeroinit" begin
    # Int8/Uint8 delegate directly to memset, but not others
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Float64}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Float32}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Float16}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Int64}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Int32}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Int16}, Int}))
    @test occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{Int8}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{UInt64}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{UInt32}, Int}))
    @test !occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{UInt16}, Int}))
    @test occursin("llvm.memset", get_llvm_unopt(zeros, Tuple{Type{UInt8}, Int}))

    if check_bounds == bc_on
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Float64}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Float32}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Float16}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int64}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int32}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int16}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int8}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt64}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt32}, Int}))
        @test_broken occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt16}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt8}, Int}))
    else
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Float64}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Float32}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Float16}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int64}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int32}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int16}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{Int8}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt64}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt32}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt16}, Int}))
        @test occursin("llvm.memset", get_llvm(zeros, Tuple{Type{UInt8}, Int}))
    end
end

@testset "$prefix basic-loops" begin
    function sumloop(N)
        total = zero(typeof(N))
        for i=one(typeof(N)):N
            total += i
        end
        total
    end
    function simd_sumloop(N)
        total = zero(typeof(N))
        @simd for i=one(typeof(N)):N
            total += i
        end
        total
    end
    # repeated addition -> multiply
    # relies on LICM, indvars
    for tp in IntTypes
        check_llvm([("mul ", true)], sumloop, Tuple{tp})
    end
    for tp in FloatTypes
        @test !occursin("vector.body", get_llvm(sumloop, Tuple{tp}))
        if tp != Float64
            @test occursin("vector.body", get_llvm(simd_sumloop, Tuple{tp}))
        else
            @test_broken occursin("vector.body", get_llvm(simd_sumloop, Tuple{tp}))
        end
    end

    function loopedlength(arr)
        len = length(arr)
        for i=1:length(arr)
            len = length(arr)
        end
        len
    end
    # length gets hoisted out of loop and loop gets deleted
    # Relies on LICM, GVN, loop-deletion
    for tp in IntTypes
        check_llvm([("br ", false)], loopedlength, Tuple{Vector{tp}})
    end
    # @test occursin("br ", get_llvm_unopt(loopedlength, Tuple{Vector{Int64}}))
    # @test !occursin("br ", get_llvm(loopedlength, Tuple{Vector{Int64}}))

    function buildarray(arr)
        for i=1:10
            @inbounds arr[i] += one(eltype(arr))
        end
    end
    # loop gets fully unrolled for short size
    # Relies on LICM, GVN, loop-unroll
    @test occursin("br ", get_llvm_unopt(buildarray, Tuple{Vector{Int64}}))
    if check_bounds != bc_on
        @test !occursin("br ", get_llvm(buildarray, Tuple{Vector{Int64}}))
    else
        @test_broken !occursin("br ", get_llvm(buildarray, Tuple{Vector{Int64}}))
    end
end

@testset "$prefix allocation optimization" begin
    count(needle, haystack) = length(collect(eachmatch(needle, haystack)))

    function split(maybe)
        if maybe
            Ref(1)
        else
            Ref(2)
        end
    end
    # optimization hoists the allocation and merges them into a single one
    # Relies on simplifycfg's hoisting parameter
    @test count(r"alloc", get_llvm_unopt(split, Tuple{Bool})) == 2
    @test count(r"alloc", get_llvm(split, Tuple{Bool})) == 1

    function loop_alloc(N)
        ref = Ref(zero(typeof(N)))
        N <= zero(typeof(N)) && return ref
        for i=one(typeof(N)):N
            ref = Ref(i)
        end
        ref
    end

    # JuliaLICM should hoist out the allocation from the loop,
    # so the only phi node should select between N <= 0 and not
    # Relies on LICM, JuliaLICM
    for tp in IntTypes
        @test count(r"phi", get_llvm_unopt(loop_alloc, Tuple{tp})) > 1
        @test count(r"phi", get_llvm(loop_alloc, Tuple{tp})) == 1
    end

    function loop_const()
        ref = Ref(0)
        for i=1:1000
            ref = Ref(0)
        end
        ref
    end

    # Relies on AllocOpt to totally elide the branch and delete the allocation
    check_llvm([("br ", false)], loop_const, Tuple{})

    function nopreserve()
        ref = Ref(0)
        GC.@preserve ref begin
        end
    end

    # Relies on AllocOpt to remove the gc_preserve intrinsics and delete the allocation
    check_llvm([("alloc", false), ("julia.gc_preserve_begin", false), ("julia.gc_preserve_end", false)], nopreserve, Tuple{})
end
