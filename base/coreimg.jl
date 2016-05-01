# This file is a part of Julia. License is MIT: http://julialang.org/license

Main.Core.eval(Main.Core, :(baremodule Inference
using Core.TopModule, Core.Intrinsics
ccall(:jl_set_istopmod, Void, (Bool,), false)

eval(x) = Core.eval(Inference,x)
eval(m,x) = Core.eval(m,x)

include = Core.include

# simple print definitions for debugging.
show(x::ANY) = ccall(:jl_static_show, Void, (Ptr{Void}, Any),
                     pointerref(cglobal(:jl_uv_stdout,Ptr{Void}),1), x)
print(x::ANY) = show(x)
println(x::ANY) = ccall(:jl_, Void, (Any,), x) # includes a newline
print(a::ANY...) = for x=a; print(x); end

## Load essential files and libraries
include("essentials.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

# core operations & types
typealias Cint Int32
typealias Csize_t UInt
include("promotion.jl")
include("tuple.jl")
include("range.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
const checked_add = +
const checked_sub = -
if !isdefined(Main, :Base)
    # conditional to allow redefining Core.Inference after base exists
    (::Type{T}){T}(arg) = convert(T, arg)::T
end

# core array operations
include("abstractarray.jl")
include("array.jl")

#TODO: eliminate Dict from inference
include("hashing.jl")
include("nofloat_hashing.jl")

# map-reduce operators
macro simd(forloop)
    esc(forloop)
end
include("reduce.jl")

## core structures
include("intset.jl")
include("dict.jl")
include("iterator.jl")

# core docsystem
include("docs/core.jl")

# compiler
include("inference.jl")

end # baremodule Inference
))
