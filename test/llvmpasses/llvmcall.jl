# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t
# RUN: cat %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

@generated foo(x)=:(ccall("extern foo", llvmcall, $x, ($x,), x))

# CHECK: call half @foo(half zeroext %{{[0-9]+}})
emit(foo, Float16)

# CHECK: call [2 x half] @foo([2 x half] %{{[0-9]+}})
emit(foo, NTuple{2, Float16})

# CHECK: call <2 x half> @foo(<2 x half> %{{[0-9]+}})
emit(foo, NTuple{2, VecElement{Float16}})
