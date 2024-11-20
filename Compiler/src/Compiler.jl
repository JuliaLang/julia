# This file is a part of Julia. License is MIT: https://julialang.org/license

if isdefined(Base, :end_base_include) && !isdefined(Base, :Compiler)

# Define a dummy `Compiler` module to make it installable even on Julia versions where
# Compiler.jl is not available as a standard library.
@eval module Compiler
    function __init__()
        println("""
        The `Compiler` standard library is not available for this version of Julia.
        Use Julia version `v"1.12.0-DEV.1581"` or later.
        """)
    end
end

# When generating an incremental precompile file, we first check whether we
# already have a copy of this *exact* code in the system image. If so, we
# simply generates a pkgimage that has the dependency edges we recorded in
# the system image and simply returns that copy of the compiler. If not,
# we proceed to load/precompile this as an ordinary package.
elseif (isdefined(Base, :generating_output) && Base.generating_output(true) &&
        Base.samefile(joinpath(Sys.BINDIR, Base.DATAROOTDIR, Base._compiler_require_dependencies[1][2]), @eval @__FILE__) &&
        !Base.any_includes_stale(
            map(Base.compiler_chi, Base._compiler_require_dependencies),
            "sysimg", nothing))

    Base.prepare_compiler_stub_image!()
    append!(Base._require_dependencies, map(Base.expand_compiler_path, Base._compiler_require_dependencies))
    # There isn't much point in precompiling native code - downstream users will
    # specialize their own versions of the compiler code and we don't activate
    # the compiler by default anyway, so let's save ourselves some disk space.
    ccall(:jl_suppress_precompile, Cvoid, (Cint,), 1)

else

@eval baremodule Compiler

# Needs to match UUID defined in Project.toml
ccall(:jl_set_module_uuid, Cvoid, (Any, NTuple{2, UInt64}), Compiler,
    (0x807dbc54_b67e_4c79, 0x8afb_eafe4df6f2e1))

using Core.Intrinsics, Core.IR

import Core: print, println, show, write, unsafe_write,
             _apply_iterate, svec, apply_type, Builtin, IntrinsicFunction,
             MethodInstance, CodeInstance, MethodTable, MethodMatch, PartialOpaque,
             TypeofVararg, Core, SimpleVector, donotdelete, compilerbarrier,
             memoryref_isassigned, memoryrefnew, memoryrefoffset, memoryrefget,
             memoryrefset!, typename

using Base
using Base: Ordering, vect, EffectsOverride, BitVector, @_gc_preserve_begin, @_gc_preserve_end, RefValue,
    @nospecializeinfer, @_foldable_meta, fieldindex, is_function_def, indexed_iterate, isexpr, methods,
    get_world_counter, JLOptions, _methods_by_ftype, unwrap_unionall, cconvert, unsafe_convert,
    issingletontype, isType, rewrap_unionall, has_free_typevars, isvarargtype, hasgenerator,
    IteratorSize, SizeUnknown, _array_for, Bottom, generating_output, diff_names,
    ismutationfree, NUM_EFFECTS_OVERRIDES, _NAMEDTUPLE_NAME, datatype_fieldtypes,
    argument_datatype, isfieldatomic, unwrapva, iskindtype, _bits_findnext, copy_exprargs,
    Generator, Filter, ismutabletypename, isvatuple, datatype_fieldcount,
    isconcretedispatch, isdispatchelem, datatype_layoutsize,
    datatype_arrayelem, unionlen, isidentityfree, _uniontypes, uniontypes, OneTo, Callable,
    DataTypeFieldDesc, datatype_nfields, datatype_pointerfree, midpoint, is_valid_intrinsic_elptr,
    allocatedinline, isbitsunion, widen_diagonal, unconstrain_vararg_length,
    rename_unionall, may_invoke_generator, is_meta_expr_head, is_meta_expr, quoted,
    specialize_method, hasintersect, is_nospecializeinfer, is_nospecialized,
    get_nospecializeinfer_sig, tls_world_age, uniontype_layout, kwerr,
    moduleroot, is_file_tracked, decode_effects_override, lookup_binding_partition,
    is_some_imported, binding_kind, is_some_guard, is_some_const_binding, partition_restriction,
    BINDING_KIND_GLOBAL, structdiff
using Base.Order
import Base: getindex, setindex!, length, iterate, push!, isempty, first, convert, ==,
    copy, popfirst!, in, haskey, resize!, copy!, append!, last, get!, size,
    get, iterate, findall, min_world, max_world, _topmod, isready

const getproperty = Core.getfield
const setproperty! = Core.setfield!
const swapproperty! = Core.swapfield!
const modifyproperty! = Core.modifyfield!
const replaceproperty! = Core.replacefield!
const _DOCS_ALIASING_WARNING = ""

ccall(:jl_set_istopmod, Cvoid, (Any, Bool), Compiler, false)

eval(x) = Core.eval(Compiler, x)
eval(m, x) = Core.eval(m, x)

function include(x::String)
    if !isdefined(Base, :end_base_include)
        # During bootstrap, all includes are relative to `base/`
        x = Base.strcat(Base.strcat(Base.DATAROOT, "julia/Compiler/src/"), x)
    end
    Base.include(Compiler, x)
end

function include(mod::Module, x::String)
    if !isdefined(Base, :end_base_include)
        x = Base.strcat(Base.strcat(Base.DATAROOT, "julia/Compiler/src/"), x)
    end
    Base.include(mod, x)
end

macro _boundscheck() Expr(:boundscheck) end

function return_type end
function is_return_type(Core.@nospecialize(f))
    f === return_type && return true
    if isdefined(Base, :Compiler) && Compiler !== Base.Compiler
        # Also model the return_type function of the builtin Compiler the same.
        # This isn't completely sound. We don't actually have any idea what the
        # base compiler will do at runtime. In the fullness of time, we should
        # re-work the semantics to make the cache primary and thus avoid having
        # to reason about what the compiler may do at runtime, but we're not
        # fully there yet.
        return f === Base.Compiler.return_type
    end
    return false
end

include("sort.jl")

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

include("cicache.jl")
include("methodtable.jl")
include("effects.jl")
include("types.jl")
include("utilities.jl")
include("validation.jl")

include("ssair/basicblock.jl")
include("ssair/domtree.jl")
include("ssair/ir.jl")
include("ssair/tarjan.jl")

include("abstractlattice.jl")
include("stmtinfo.jl")
include("inferenceresult.jl")
include("inferencestate.jl")

include("typeutils.jl")
include("typelimits.jl")
include("typelattice.jl")
include("tfuncs.jl")

include("abstractinterpretation.jl")
include("typeinfer.jl")
include("optimize.jl")

include("bootstrap.jl")
include("reflection_interface.jl")
include("opaque_closure.jl")

module IRShow end
if !isdefined(Base, :end_base_include)
    # During bootstrap, skip including this file and defer it to base/show.jl to include later
else
    # When this module is loaded as the standard library, include this file as usual
    include(IRShow, "ssair/show.jl")
end

end # baremodule Compiler

end # if isdefined(Base, :generating_output) && ...
