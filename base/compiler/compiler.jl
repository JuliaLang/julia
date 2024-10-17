# This file is a part of Julia. License is MIT: https://julialang.org/license


baremodule Compiler

using Core.Intrinsics, Core.IR

import Core: print, println, show, write, unsafe_write,
             _apply_iterate, svec, apply_type, Builtin, IntrinsicFunction,
             MethodInstance, CodeInstance, MethodTable, MethodMatch, PartialOpaque,
             TypeofVararg, Core, SimpleVector, donotdelete, compilerbarrier,
             memoryref_isassigned, memoryrefnew, memoryrefoffset, memoryrefget,
             memoryrefset!, typename

using ..Base
using ..Base: Ordering, vect, EffectsOverride, BitVector, @_gc_preserve_begin, @_gc_preserve_end, RefValue,
    @nospecializeinfer, @_foldable_meta, fieldindex, is_function_def, indexed_iterate, isexpr, methods,
    get_world_counter, JLOptions, _methods_by_ftype, unwrap_unionall, cconvert, unsafe_convert,
    issingletontype, isType, rewrap_unionall, has_free_typevars, isvarargtype, hasgenerator,
    IteratorSize, SizeUnknown, _array_for, Bottom, generating_output, diff_names,
    ismutationfree, NUM_EFFECTS_OVERRIDES, _NAMEDTUPLE_NAME, datatype_fieldtypes,
    argument_datatype, isfieldatomic, unwrapva, iskindtype, _bits_findnext, copy_exprargs,
    Generator, Filter, ismutabletypename, isvatuple, datatype_fieldcount,
    isconcretedispatch, isdispatchelem, min_world, max_world, datatype_layoutsize,
    datatype_arrayelem, unionlen, isidentityfree, _uniontypes, uniontypes, OneTo, Callable,
    DataTypeFieldDesc, datatype_nfields, datatype_pointerfree, midpoint, is_valid_intrinsic_elptr,
    allocatedinline, isbitsunion, widen_diagonal, unconstrain_vararg_length,
    rename_unionall, may_invoke_generator, is_meta_expr_head, is_meta_expr, quoted,
    specialize_method, hasintersect, is_nospecializeinfer, is_nospecialized,
    get_nospecializeinfer_sig, tls_world_age, uniontype_layout, kwerr,
    moduleroot, is_file_tracked, decode_effects_override
using ..Base.Order
import ..Base: getindex, setindex!, length, iterate, push!, isempty, first, convert, ==,
    copy, popfirst!, in, haskey, resize!, copy!, append!, last, get!, size,
    get, iterate, findall

const getproperty = Core.getfield
const setproperty! = Core.setfield!
const swapproperty! = Core.swapfield!
const modifyproperty! = Core.modifyfield!
const replaceproperty! = Core.replacefield!
const _DOCS_ALIASING_WARNING = ""

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Compiler, false)

eval(x) = Core.eval(Compiler, x)
eval(m, x) = Core.eval(m, x)

include(x) = Base.include(Compiler, x)
include(mod, x) = Base.include(mod, x)

macro _boundscheck() Expr(:boundscheck) end

# These types are used by reflection.jl and expr.jl too, so declare them here.
# Note that `@assume_effects` is available only after loading namedtuple.jl.
abstract type MethodTableView end
abstract type AbstractInterpreter end

function return_type end # promotion.jl expects this to exist
is_return_type(Core.@nospecialize(f)) = f === return_type

include("compiler/sort.jl")

# We don't include some.jl, but this definition is still useful.
something(x::Nothing, y...) = something(y...)
something(x::Any, y...) = x

############
# compiler #
############

baremodule BuildSettings
using Core: ARGS, include
using ..Compiler: >, getindex, length

global MAX_METHODS::Int = 3

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

end
