# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t
# RUN: cat %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

struct Foo
    x::Int32
    y::Int32
end

@generated foo(x)=:(ccall("extern foo", llvmcall, $x, ($x,), x))
bar(x) = ntuple(i -> VecElement{Float16}(x[i]), 2)

# CHECK: call half @foo(half zeroext %{{[0-9]+}})
emit(foo, Float16)

# CHECK: call [2 x half] @foo([2 x half] %{{[0-9]+}})
emit(foo, NTuple{2, Float16})

# CHECK: call <2 x half> @foo(<2 x half> %{{[0-9]+}})
emit(foo, NTuple{2, VecElement{Float16}})

# CHECK: call i8 addrspace(3)* @foo(i8 addrspace(3)* %{{[0-9]+}})
emit(foo, Core.LLVMPtr{Float32, 3})

# CHECK: call { i32, i32 } @foo({ i32, i32 } %{{[0-9]+}})
emit(foo, Foo)

# CHECK: define <2 x half> @julia_bar_{{[0-9]+}}([2 x half]
emit(bar, NTuple{2, Float16})
