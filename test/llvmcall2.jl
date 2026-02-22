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

@testset "JLJIT API" begin
    function JLJITGetJuliaOJIT()
        ccall(:JLJITGetJuliaOJIT, Ptr{Cvoid}, ())
    end
    function JLJITGetTripleString(JIT)
        ccall(:JLJITGetTripleString, Cstring, (Ptr{Cvoid},), JIT)
    end
    jit = JLJITGetJuliaOJIT()
    str = JLJITGetTripleString(jit)
    jl_str = unsafe_string(str)
    @test length(jl_str) > 4
end


# boolean structs
const NT4I = NTuple{4, VecElement{Int}}
const NT4B = NTuple{4, VecElement{Bool}}
f_nt4b(x, y) = ccall("llvm.sadd.with.overflow", llvmcall, Pair{NT4B, NT4B}, (NT4B, NT4B), x, y)
f_nt4i(x, y) = ccall("llvm.sadd.with.overflow", llvmcall, Pair{NT4I, NT4B}, (NT4I, NT4I), x, y)
@test f_nt4b((false, true, false, true), (false, false, true, true)) === (NT4B((false, true, true, false)) => NT4B((false, false, false, true)))
@test f_nt4i((typemin(Int), 0, typemax(Int), typemax(Int)), (-1, typemax(Int),-1, 1)) === (NT4I((typemax(Int), typemax(Int), typemax(Int)-1, typemin(Int))) => NT4B((true, false, false, true)))
