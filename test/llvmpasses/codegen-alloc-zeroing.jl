# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for GC pointer zeroing operand bundles on allocation calls
# These bundles inform late-gc-lowering about which fields need to be zeroed

using Test, InteractiveUtils

# Helper to get raw LLVM IR (before late-gc-lowering)
function get_raw_llvm(@nospecialize(f), @nospecialize(tt))
    sprint((io, f, tt) -> code_llvm(io, f, tt; raw=true, optimize=false, debuginfo=:none), f, tt)
end

# Helper to get optimized LLVM IR (after late-gc-lowering)
function get_opt_llvm(@nospecialize(f), @nospecialize(tt))
    sprint((io, f, tt) -> code_llvm(io, f, tt; optimize=true, debuginfo=:none), f, tt)
end

@testset "GC allocation zeroing bundles" begin
    # Test 1: Mutable struct with pointer fields should emit julia.gc_alloc_ptr_offsets bundle
    @testset "Struct with pointer fields" begin
        mutable struct TestStructPtrs
            a::Any      # GC pointer at offset 0
            b::Int64    # Non-pointer at offset 8
            c::Any      # GC pointer at offset 16
        end

        function make_struct_ptrs(x, y, z)
            TestStructPtrs(x, y, z)
        end

        # Check raw IR has the bundle
        raw_ir = get_raw_llvm(make_struct_ptrs, Tuple{Any, Int64, Any})
        @test occursin("julia.gc_alloc_ptr_offsets", raw_ir)
        @test occursin("i64 0", raw_ir) && occursin("i64 16", raw_ir)  # offsets for 'a' and 'c'

        # Check optimized IR has null stores after allocation
        opt_ir = get_opt_llvm(make_struct_ptrs, Tuple{Any, Int64, Any})
        @test occursin("store ptr null", opt_ir) || occursin("store ptr addrspace(10) null", opt_ir)
    end

    # Test 2: Struct with only non-pointer fields should NOT have the bundle
    @testset "Struct without pointer fields" begin
        mutable struct TestStructNoPtrs
            a::Int64
            b::Int64
        end

        function make_struct_no_ptrs(x, y)
            TestStructNoPtrs(x, y)
        end

        raw_ir = get_raw_llvm(make_struct_no_ptrs, Tuple{Int64, Int64})
        # Should have allocation but NO ptr_offsets bundle
        @test occursin("julia.gc_alloc_obj", raw_ir)
        @test !occursin("julia.gc_alloc_ptr_offsets", raw_ir)
    end

    # Test 3: Memory{Any} should emit julia.gc_alloc_zeroinit bundle
    @testset "Memory with boxed elements" begin
        function make_memory_any()
            Memory{Any}(undef, 4)
        end

        # Check raw IR has the zeroinit bundle
        raw_ir = get_raw_llvm(make_memory_any, Tuple{})
        @test occursin("julia.gc_alloc_zeroinit", raw_ir)
        # Should specify offset 16 (after header) and size 32 (4 * 8 bytes)
        @test occursin("i64 16", raw_ir) && occursin("i64 32", raw_ir)

        # Check optimized IR has memset
        opt_ir = get_opt_llvm(make_memory_any, Tuple{})
        @test occursin("llvm.memset", opt_ir)
    end

    # Test 4: Memory{Int64} (non-boxed) should NOT have zeroinit bundle
    @testset "Memory with non-boxed elements" begin
        function make_memory_int()
            Memory{Int64}(undef, 4)
        end

        raw_ir = get_raw_llvm(make_memory_int, Tuple{})
        # Int64 doesn't need zeroing, so no zeroinit bundle
        @test !occursin("julia.gc_alloc_zeroinit", raw_ir)
    end

    # Test 5: Single pointer field struct
    @testset "Single pointer field" begin
        mutable struct SinglePtr
            x::Any
        end

        function make_single_ptr(x)
            SinglePtr(x)
        end

        raw_ir = get_raw_llvm(make_single_ptr, Tuple{Any})
        @test occursin("julia.gc_alloc_ptr_offsets", raw_ir)
        @test occursin("i64 0", raw_ir)  # pointer at offset 0
    end

    # Test 6: Variable-length Memory{Any} uses zeroinit_indirect bundle
    @testset "Variable-length Memory with boxed elements" begin
        function make_memory_any_dynamic(n::Int)
            Memory{Any}(undef, n)
        end

        raw_ir = get_raw_llvm(make_memory_any_dynamic, Tuple{Int})
        # Variable-length uses jl_alloc_genericmemory_unchecked with zeroinit_indirect bundle
        @test occursin("jl_alloc_genericmemory_unchecked", raw_ir)
        @test occursin("julia.gc_alloc_zeroinit_indirect", raw_ir)
        # Data pointer offset is 8 (sizeof(size_t) for length field)
        @test occursin("i64 8", raw_ir)

        # Optimized IR should have memset (emitted by late-gc-lowering)
        opt_ir = get_opt_llvm(make_memory_any_dynamic, Tuple{Int})
        @test occursin("llvm.memset", opt_ir)
    end

    # Test 7: Variable-length Memory{Int64} should NOT have zeroinit bundle
    @testset "Variable-length Memory with non-boxed elements" begin
        function make_memory_int_dynamic(n::Int)
            Memory{Int64}(undef, n)
        end

        raw_ir = get_raw_llvm(make_memory_int_dynamic, Tuple{Int})
        @test occursin("jl_alloc_genericmemory_unchecked", raw_ir)
        # Int64 doesn't need zeroing, so no zeroinit bundle
        @test !occursin("julia.gc_alloc_zeroinit_indirect", raw_ir)
    end
end
