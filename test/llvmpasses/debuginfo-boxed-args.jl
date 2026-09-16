# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

struct BoxedArgP
    x::Float64
    y::Int64
end

@noinline g(b::Vector{Int}, p::BoxedArgP, f::Float64) = length(b) + p.y + f

# A boxed argument's debug variable is the jl_value_t* itself, so it takes
# value semantics (#dbg_value); an unboxed argument passed by reference keeps
# #dbg_declare, since its bytes do live at that address.
# CHECK: #dbg_value(ptr addrspace(10) %"b::Array"
# CHECK: #dbg_declare(ptr addrspace(11) %"p::BoxedArgP"
# CHECK: #dbg_value(double %"f::Float64"
emit(g, Vector{Int}, BoxedArgP, Float64; debuginfo=:source)
