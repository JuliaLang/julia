# This file is a part of Julia. License is MIT: https://julialang.org/license

getfield(Core, :eval)(Core, :(baremodule Compiler

using Core.Intrinsics, Core.IR

import Core: print, println, show, write, unsafe_write, stdout, stderr,
             _apply_iterate, svec, apply_type, Builtin, IntrinsicFunction,
             MethodInstance, CodeInstance, MethodTable, MethodMatch, PartialOpaque,
             TypeofVararg

const getproperty = Core.getfield
const setproperty! = Core.setfield!
const swapproperty! = Core.swapfield!
const modifyproperty! = Core.modifyfield!
const replaceproperty! = Core.replacefield!
const _DOCS_ALIASING_WARNING = ""

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Compiler, false)

eval(x) = Core.eval(Compiler, x)
eval(m, x) = Core.eval(m, x)

include(x) = Core.include(Compiler, x)
include(mod, x) = Core.include(mod, x)

# The @inline/@noinline macros that can be applied to a function declaration are not available
# until after array.jl, and so we will mark them within a function body instead.
macro inline()   Expr(:meta, :inline)   end
macro noinline() Expr(:meta, :noinline) end

macro _boundscheck() Expr(:boundscheck) end

convert(::Type{Any}, Core.@nospecialize x) = x
convert(::Type{T}, x::T) where {T} = x

# These types are used by reflection.jl and expr.jl too, so declare them here.
# Note that `@assume_effects` is available only after loading namedtuple.jl.
abstract type MethodTableView end
abstract type AbstractInterpreter end
struct EffectsOverride
    consistent::Bool
    effect_free::Bool
    nothrow::Bool
    terminates_globally::Bool
    terminates_locally::Bool
    notaskstate::Bool
    inaccessiblememonly::Bool
    noub::Bool
    noub_if_noinbounds::Bool
    consistent_overlay::Bool
end
function EffectsOverride(
    override::EffectsOverride =
        EffectsOverride(false, false, false, false, false, false, false, false, false, false);
    consistent::Bool = override.consistent,
    effect_free::Bool = override.effect_free,
    nothrow::Bool = override.nothrow,
    terminates_globally::Bool = override.terminates_globally,
    terminates_locally::Bool = override.terminates_locally,
    notaskstate::Bool = override.notaskstate,
    inaccessiblememonly::Bool = override.inaccessiblememonly,
    noub::Bool = override.noub,
    noub_if_noinbounds::Bool = override.noub_if_noinbounds,
    consistent_overlay::Bool = override.consistent_overlay)
    return EffectsOverride(
        consistent,
        effect_free,
        nothrow,
        terminates_globally,
        terminates_locally,
        notaskstate,
        inaccessiblememonly,
        noub,
        noub_if_noinbounds,
        consistent_overlay)
end
const NUM_EFFECTS_OVERRIDES = 10 # sync with julia.h

# essential files and libraries
include("essentials.jl")
include("ctypes.jl")
include("generator.jl")
include("reflection.jl")
include("options.jl")

ntuple(f, ::Val{0}) = ()
ntuple(f, ::Val{1}) = (@inline; (f(1),))
ntuple(f, ::Val{2}) = (@inline; (f(1), f(2)))
ntuple(f, ::Val{3}) = (@inline; (f(1), f(2), f(3)))
ntuple(f, ::Val{n}) where {n} = ntuple(f, n::Int)
ntuple(f, n) = (Any[f(i) for i = 1:n]...,)

# core operations & types
function return_type end # promotion.jl expects this to exist
is_return_type(Core.@nospecialize(f)) = f === return_type
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

# the same constructor as defined in float.jl, but with a different name to avoid redefinition
_Bool(x::Real) = x==0 ? false : x==1 ? true : throw(InexactError(:Bool, Bool, x))
# fld(x,y) == div(x,y) - ((x>=0) != (y>=0) && rem(x,y) != 0 ? 1 : 0)
fld(x::T, y::T) where {T<:Unsigned} = div(x, y)
function fld(x::T, y::T) where T<:Integer
    d = div(x, y)
    return d - (signbit(x ⊻ y) & (d * y != x))
end
# cld(x,y) = div(x,y) + ((x>0) == (y>0) && rem(x,y) != 0 ? 1 : 0)
function cld(x::T, y::T) where T<:Unsigned
    d = div(x, y)
    return d + (d * y != x)
end
function cld(x::T, y::T) where T<:Integer
    d = div(x, y)
    return d + (((x > 0) == (y > 0)) & (d * y != x))
end

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

include("cmem.jl")
include("strings/lazy.jl")

# core array operations
include("indices.jl")
include("genericmemory.jl")
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

# core docsystem
include("docs/core.jl")
import Core.Compiler.CoreDocs
Core.atdoc!(CoreDocs.docm)

# sorting
include("ordering.jl")
using .Order
include("compiler/sort.jl")

# We don't include some.jl, but this definition is still useful.
something(x::Nothing, y...) = something(y...)
something(x::Any, y...) = x

############
# compiler #
############

baremodule BuildSettings
using Core: ARGS, include
using Core.Compiler: >, getindex, length

MAX_METHODS::Int = 3
UNOPTIMIZE_THROW_BLOCKS::Bool = true

if length(ARGS) > 2 && ARGS[2] === "--buildsettings"
    include(BuildSettings, ARGS[3])
end
end

if false
    import Base: Base, @show
else
    macro show(ex...)
        blk = Expr(:block)
        for s in ex
            push!(blk.args, :(println(stdout, $(QuoteNode(s)), " = ",
                                              begin local value = $(esc(s)) end)))
        end
        isempty(ex) || push!(blk.args, :value)
        blk
    end
end

include("compiler/cicache.jl")
include("compiler/methodtable.jl")
include("compiler/effects.jl")
include("compiler/types.jl")
include("compiler/utilities.jl")
include("compiler/validation.jl")

include("compiler/ssair/basicblock.jl")
include("compiler/ssair/domtree.jl")
include("compiler/ssair/ir.jl")
include("compiler/ssair/tarjan.jl")

include("compiler/abstractlattice.jl")
include("compiler/stmtinfo.jl")
include("compiler/inferenceresult.jl")
include("compiler/inferencestate.jl")

include("compiler/typeutils.jl")
include("compiler/typelimits.jl")
include("compiler/typelattice.jl")
include("compiler/tfuncs.jl")

include("compiler/abstractinterpretation.jl")
include("compiler/typeinfer.jl")
include("compiler/optimize.jl")

include("compiler/bootstrap.jl")
ccall(:jl_set_typeinf_func, Cvoid, (Any,), typeinf_ext_toplevel)

include("compiler/parsing.jl")
Core._setparser!(fl_parse)

end # baremodule Compiler
))
