# This file is a part of Julia. License is MIT: https://julialang.org/license

getfield(Core, :eval)(Core, :(baremodule Compiler

using Core.Intrinsics, Core.IR

import Core: print, println, show, write, unsafe_write, stdout, stderr,
             _apply_iterate, svec, apply_type, Builtin, IntrinsicFunction,
             MethodInstance, CodeInstance, MethodMatch, PartialOpaque

const getproperty = Core.getfield
const setproperty! = Core.setfield!
const swapproperty! = Core.swapfield!
const modifyproperty! = Core.modifyfield!
const replaceproperty! = Core.replacefield!

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Compiler, false)

eval(x) = Core.eval(Compiler, x)
eval(m, x) = Core.eval(m, x)

include(x) = Core.include(Compiler, x)
include(mod, x) = Core.include(mod, x)

# The @inline/@noinline macros that can be applied to a function declaration are not available
# until after array.jl, and so we will mark them within a function body instead.
macro inline()   Expr(:meta, :inline)   end
macro noinline() Expr(:meta, :noinline) end

# essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

# core operations & types
function return_type end # promotion.jl expects this to exist
is_return_type(@Core.nospecialize(f)) = f === return_type
include("promotion.jl")
include("tuple.jl")
include("pair.jl")
include("traits.jl")
include("range.jl")
include("expr.jl")
include("error.jl")

# core numeric operations & types
==(x::T, y::T) where {T} = x === y
include("bool.jl")
include("number.jl")
include("int.jl")
include("operators.jl")
include("pointer.jl")
include("refvalue.jl")

# checked arithmetic
const checked_add = +
const checked_sub = -
const SignedInt = Union{Int8,Int16,Int32,Int64,Int128}
const UnsignedInt = Union{UInt8,UInt16,UInt32,UInt64,UInt128}
sub_with_overflow(x::T, y::T) where {T<:SignedInt}   = checked_ssub_int(x, y)
sub_with_overflow(x::T, y::T) where {T<:UnsignedInt} = checked_usub_int(x, y)
sub_with_overflow(x::Bool, y::Bool) = (x-y, false)
add_with_overflow(x::T, y::T) where {T<:SignedInt}   = checked_sadd_int(x, y)
add_with_overflow(x::T, y::T) where {T<:UnsignedInt} = checked_uadd_int(x, y)
add_with_overflow(x::Bool, y::Bool) = (x+y, false)

# core array operations
include("indices.jl")
include("array.jl")
include("abstractarray.jl")

# core structures
include("bitarray.jl")
include("bitset.jl")
include("abstractdict.jl")
include("iddict.jl")
include("idset.jl")
include("abstractset.jl")
include("iterators.jl")
using .Iterators: zip, enumerate
using .Iterators: Flatten, Filter, product  # for generators
include("namedtuple.jl")

ntuple(f, ::Val{0}) = ()
ntuple(f, ::Val{1}) = (@inline; (f(1),))
ntuple(f, ::Val{2}) = (@inline; (f(1), f(2)))
ntuple(f, ::Val{3}) = (@inline; (f(1), f(2), f(3)))
ntuple(f, ::Val{n}) where {n} = ntuple(f, n::Int)
ntuple(f, n) = (Any[f(i) for i = 1:n]...,)

# core docsystem
include("docs/core.jl")

# sorting
function sort end
function sort! end
function issorted end
function sortperm end
include("ordering.jl")
using .Order
include("sort.jl")
using .Sort

# We don't include some.jl, but this definition is still useful.
something(x::Nothing, y...) = something(y...)
something(x::Any, y...) = x

############
# compiler #
############

include("compiler/cicache.jl")
include("compiler/types.jl")
include("compiler/utilities.jl")
include("compiler/validation.jl")
include("compiler/methodtable.jl")

include("compiler/inferenceresult.jl")
include("compiler/inferencestate.jl")

include("compiler/typeutils.jl")
include("compiler/typelimits.jl")
include("compiler/typelattice.jl")
include("compiler/tfuncs.jl")
include("compiler/stmtinfo.jl")

include("compiler/abstractinterpretation.jl")
include("compiler/typeinfer.jl")
include("compiler/optimize.jl") # TODO: break this up further + extract utilities

include("compiler/bootstrap.jl")
ccall(:jl_set_typeinf_func, Cvoid, (Any,), typeinf_ext_toplevel)

include("compiler/parsing.jl")
Core.eval(Core, :(_parse = Compiler.fl_parse))

end # baremodule Compiler
))
