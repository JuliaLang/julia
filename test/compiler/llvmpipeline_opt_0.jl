# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using InteractiveUtils

@enum BCOption bc_default bc_on bc_off
opts = Base.JLOptions()
check_bounds = BCOption(opts.check_bounds)
opt_level = opts.opt_level

if check_bounds == bc_on
    bc = "yes"
elseif check_bounds == bc_off
    bc = "no"
else
    bc = "auto"
end
prefix = "O$opt_level bounds=$bc;"

get_llvm(@nospecialize(f), @nospecialize(t), raw=true, dump_module=false, optimize=true) =
    sprint(code_llvm, f, t, raw, dump_module, optimize)
get_llvm_unopt(@nospecialize(f), @nospecialize(t), raw=true, dump_module=false) =
    get_llvm(f, t, raw, dump_module, false)

IntTypes = (Int32, Int64, UInt32, UInt64)
FloatTypes = (Float16, Float32, Float64)

function check_llvm(occurences::Vector{Tuple{String, Bool}}, @nospecialize(f), @nospecialize(t))
    unopt = get_llvm_unopt(f, t)
    opt = get_llvm(f, t)
    for (needle, optimized) in occurences
        @test occursin(needle, opt) == optimized
        @test occursin(needle, unopt) != optimized
    end
end

# These tests act as a simple guard to ensure we don't break our pass pipeline
# They are NOT a promise of any kind of performance for any particular syntax
# Tests may be marked as broken at any time depending on the PR

# The file is organized into a few basic patterns of optimizations we
# want to occur, namely:
# * GC lowering
# * SIMD loop marker removal
# * PTLS lowering

# @testset "$prefix ptls lowering" begin

function simple()
    Ref(0)
end
# TLS load + object allocation
check_llvm([("asm", true), ("julia.get_pgcstack", false), ("julia.gc_alloc_obj", false), ("ijl_gc_pool_alloc", true)], simple, Tuple{})

# end

# @testset "$prefix gc lowering" begin

function buildarray()
    out = []
    for i=1:100
        push!(out, Ref(0))
    end
    out
end
# Write barrier lowering
check_llvm([("gc_queue_root", true), ("julia.write_barrier", false)], buildarray, Tuple{})

# end

# @testset "$prefix simd lowering" begin

function simd_loop()
    total = 0.0
    @simd for i=1:1000
        total += i
    end
    total
end
check_llvm([("fadd fast", true), ("julia.loopinfo_marker", false)], simd_loop, Tuple{})

# end
