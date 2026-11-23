# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

struct BoxedArgP
    x::Float64
    y::Int64
end

mutable struct BoxedArgM
    x::Float64
    y::Int64
end

@noinline g(b::Vector{Int}, m::BoxedArgM, p::BoxedArgP, f::Float64) = length(b) + m.y + p.y + f
@noinline h(x::Int, v::Vector{Int}, ys...) = x + length(v) + length(ys)

# An Array argument's debug type is a typedef of jl_value_t*, so the box
# pointer is its value (#dbg_value). A mutable struct's debug type describes
# the object, so the box is its address (#dbg_declare), as it is for an
# unboxed struct passed by reference.
# CHECK-LABEL: define {{.*}} @julia_g_
# CHECK: #dbg_value(ptr addrspace(10) %"b::Array"
# CHECK: #dbg_declare(ptr addrspace(10) %"m::BoxedArgM"
# CHECK: #dbg_declare(ptr addrspace(11) %"p::BoxedArgP"
# CHECK: #dbg_value(double %"f::Float64"
emit(g, Vector{Int}, BoxedArgM, BoxedArgP, Float64; debuginfo=:source)

# jlcall arguments are described once, through the stack copy of the
# argument array: an Int64 takes one more dereference to reach the bits in
# its box, an Array does not.
# CHECK-LABEL: define {{.*}} @japi1_h_
# CHECK-NOT: #dbg_value
# CHECK: #dbg_declare(ptr %stackargs, [[X:![0-9]+]], !DIExpression(DW_OP_deref, DW_OP_plus_uconst, 0, DW_OP_deref)
# CHECK-NOT: #dbg_{{.*}}, [[X]],
# CHECK: #dbg_declare(ptr %stackargs, [[V:![0-9]+]], !DIExpression(DW_OP_deref, DW_OP_plus_uconst, {{[48]}})
# CHECK-NOT: #dbg_{{.*}}, [[V]],
# CHECK: ret
emit(h, Int, Vector{Int}, Vararg{Any}; debuginfo=:source)
