# This file is a part of Julia. License is MIT: https://julialang.org/license

using InteractiveUtils: code_llvm

function declared_floor(x::Float64)
    return ccall("llvm.floor.f64", llvmcall, Float64, (Float64,), x)
end
@test declared_floor(4.2) == 4.0
ir = sprint(code_llvm, declared_floor, Tuple{Float64})
@test occursin("call double @llvm.floor.f64", ir) # should be inlined

function doubly_declared_floor(x::Float64)
    a = ccall("llvm.floor.f64", llvmcall, Float64, (Float64,), x)
    b = ccall("llvm.floor.f64", llvmcall, Float64, (Float64,), x + 1) - 1
    return a + b
end
@test doubly_declared_floor(4.2) == 8.0

function doubly_declared2_trunc(x::Float64)
    a = ccall("llvm.trunc.f64", llvmcall, Float64, (Float64,), x)
    b = ccall("llvm.trunc.f64", llvmcall, Float64, (Float64,), x + 1) - 1
    return a + b
end
@test doubly_declared2_trunc(4.2) == 8.0

# Test for single line
function declared_ceil(x::Float64)
    return ccall("llvm.ceil.f64", llvmcall, Float64, (Float64,), x)
end
@test declared_ceil(4.2) == 5.0

# Test for multiple lines
function ceilfloor(x::Float64)
    a = ccall("llvm.ceil.f64", llvmcall, Float64, (Float64,), x)
    b = ccall("llvm.floor.f64", llvmcall, Float64, (Float64,), a)
    return b
end
@test ceilfloor(7.4) == 8.0

let err = ErrorException("llvmcall only supports intrinsic calls")
    # support for calling external functions
    @test_throws err @eval ccall("time", llvmcall, Cvoid, (Ptr{Cvoid},), C_NULL)
    g() = ccall("extern time", llvmcall, Cvoid, (Ptr{Cvoid},), C_NULL)
    g()
    @test_throws err @eval ccall("extern llvm.floor", llvmcall, Float64, (Float64,), 0.0)

    # support for mangling
    @test (@eval ccall("llvm.floor.f64", llvmcall, Float64, (Float64,), 0.0)) === 0.0
    @test (@eval ccall("llvm.floor", llvmcall, Float64, (Float64,), 0.0),
                 ccall("llvm.floor", llvmcall, Float32, (Float32,), 0.0)) === (0.0, 0.0f0)
    @test_throws err @eval ccall("llvm.floor.f64", llvmcall, Float32, (Float64,), 0.0)
    @test_throws err @eval ccall("llvm.floor.f64", llvmcall, Float32, (Float32,), 0.0f0)
    @test_throws err @eval ccall("llvm.floor.f64", llvmcall, Float64, (Float32,), 0.0f0)
    @test_throws err @eval ccall("llvm.floor.f64", llvmcall, Float64, (Int,), 0)
    @test_throws err @eval ccall("llvm.floor.f64", llvmcall, Int, (Int,), 0)
    @test_throws err @eval ccall("llvm.floor", llvmcall, Float64, (Float32,), 0.0f0)
    @test_throws err @eval ccall("llvm.floor", llvmcall, Float64, (Int,), 0)
    @test_throws err @eval ccall("llvm.floor", llvmcall, Int, (Int,), 0)

    @test_throws err (@eval ccall("llvm.floor.f64", llvmcall, Float64, (Float64, Float64...,), 0.0)) === 0.0
    @test_throws err (@eval ccall("llvm.floor", llvmcall, Float64, (Float64, Float64...,), 0.0)) === 0.0
end
