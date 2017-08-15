# This file is a part of Julia. License is MIT: https://julialang.org/license

Main.Core.eval(Main.Core, :(baremodule Inference
using Core.Intrinsics
import Core: print, println, show, write, unsafe_write, STDOUT, STDERR

ccall(:jl_set_istopmod, Void, (Any, Bool), Inference, false)

eval(x) = Core.eval(Inference, x)
eval(m, x) = Core.eval(m, x)

include(x) = Core.include(Inference, x)
include(mod, x) = Core.include(mod, x)

function return_type end

## Load essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

# core operations & types
include("promotion.jl")
include("tuple.jl")
include("pair.jl")
include("traits.jl")
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

# core array operations
include("indices.jl")
include("array.jl")
include("abstractarray.jl")

# map-reduce operators
macro simd(forloop)
    esc(forloop)
end
include("reduce.jl")

## core structures
include("bitarray.jl")
include("bitset.jl")
include("associative.jl")

# core docsystem
include("docs/core.jl")

# compiler
include("codevalidation.jl")
include("inference.jl")
ccall(:jl_set_typeinf_func, Void, (Any,), typeinf_ext)

end # baremodule Inference
))
