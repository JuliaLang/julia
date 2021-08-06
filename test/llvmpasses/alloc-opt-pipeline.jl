# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t -O
# RUN: cat %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# JuliaLang/julia#38922
function haszerolayout(x::NTuple{32, VecElement{UInt8}})
    rx = Ref(x)
    GC.@preserve rx begin
        lower = iszero(unsafe_load(Ptr{UInt128}(pointer_from_objref(rx)), 1))
        upper = iszero(unsafe_load(Ptr{UInt128}(pointer_from_objref(rx)), 2))
        lower & upper
    end
end

# CHECK-LABEL: @julia_haszerolayout
# CHECK: top:
# CHECK-NOT: @jl_gc_pool_alloc
# CHECK: extractelement
# CHECK: ret i8
emit(haszerolayout, NTuple{32,VecElement{UInt8}})
