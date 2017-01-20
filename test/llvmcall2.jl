# This file is a part of Julia. License is MIT: http://julialang.org/license

function declared_floor(x::Float64)
    return ccall("llvm.floor.f64", llvmcall, Float64, (Float64,), x)
end
@test declared_floor(4.2) == 4.0
ir = sprint(io->code_llvm(io, declared_floor, Tuple{Float64}))
@test contains(ir, "call double @llvm.floor.f64") # should be inlined

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
