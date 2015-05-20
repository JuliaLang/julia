# This file is a part of Julia. License is MIT: http://julialang.org/license

Main.Core.eval(Main.Core, :(baremodule Inference
using Core: Intrinsics, arraylen, arrayref, arrayset, arraysize, _expr,
            kwcall, _apply, typeassert, apply_type, svec
ccall(:jl_set_istopmod, Void, (Bool,), false)

eval(x) = Core.eval(Inference,x)
eval(m,x) = Core.eval(m,x)

include = Core.include

# simple print definitions for debugging.
show(x::ANY) = ccall(:jl_static_show, Void, (Ptr{Void}, Any),
                     Intrinsics.pointerref(Intrinsics.cglobal(:jl_uv_stdout,Ptr{Void}),1), x)
print(x::ANY) = show(x)
println(x::ANY) = ccall(:jl_, Void, (Any,), x) # includes a newline
print(a::ANY...) = for x=a; print(x); end

## Load essential files and libraries
include("essentials.jl")
include("reflection.jl")
include("build_h.jl")
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

# core array operations
include("abstractarray.jl")
include("subarray.jl")
include("array.jl")
include("subarray2.jl")

#TODO: eliminate Dict from inference
include("hashing.jl")
include("nofloat_hashing.jl")

# map-reduce operators
macro simd(forloop)
    esc(forloop)
end
include("functors.jl")
include("reduce.jl")

## core structures
include("intset.jl")
include("dict.jl")
include("iterator.jl")

# compiler
include("inference.jl")

precompile(CallStack, (Expr, Module, (Void,), EmptyCallStack))
precompile(_ieval, (Symbol,))
precompile(abstract_eval, (LambdaStaticData, ObjectIdDict, StaticVarInfo))
precompile(abstract_interpret, (Bool, ObjectIdDict, StaticVarInfo))
precompile(delete_var!, (Expr, Symbol))
precompile(eval_annotate, (LambdaStaticData, ObjectIdDict, StaticVarInfo, ObjectIdDict, Array{Any,1}))
precompile(is_var_assigned, (Expr, Symbol))
precompile(isconstantfunc, (SymbolNode, StaticVarInfo))
precompile(occurs_more, (Bool, Function, Int))
precompile(occurs_more, (UInt8, Function, Int))
precompile(occurs_undef, (Symbol, Expr))
precompile(sym_replace, (UInt8, Array{Any,1}, Array{Any,1}, Array{Any,1}, Array{Any,1}))
precompile(symequal, (Symbol, Symbol))

end # baremodule Inference
))
